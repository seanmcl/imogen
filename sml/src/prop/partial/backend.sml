
structure Backend :> Backend = struct
   structure C = Focus
   structure Node = PDB.Node
   structure S = BackendUtil.Status

   open General
   open PP.Ops

   structure Stats = struct
      datatype one = T1 of
         { iterations: int
         , activeSeqs: int
         , keptSeqs: int
         , activeRules: int
         , keptRules: int
         }

      fun pp1 (T1 { iterations, activeSeqs, keptSeqs, activeRules, keptRules }) =
         %[ %[$"i: ",PP.int iterations]
          , %[$", as: ",PP.int activeSeqs]
          , %[$", ks: ",PP.int keptSeqs]
          , %[$", ar: ",PP.int activeRules]
          , %[$", kr: ",PP.int keptRules]
          ]

      datatype t = T of
         { maxIterations: int
         , totalIterations: int
         , maxActiveSeqs: int
         , maxKeptSeqs: int
         , maxActiveRules: int
         , maxKeptRules: int
         }

      val empty =
         T { maxIterations = 0
           , totalIterations = 0
           , maxActiveSeqs = 0
           , maxKeptSeqs = 0
           , maxActiveRules = 0
           , maxKeptRules = 0
           }

      fun add ( T { maxIterations, totalIterations, maxActiveSeqs
                  , maxKeptSeqs, maxActiveRules, maxKeptRules}
              , T1 {iterations, activeSeqs, keptSeqs,
                     activeRules, keptRules}) =
         T { maxIterations = Int.max(maxIterations, iterations)
           , totalIterations = iterations + totalIterations
           , maxActiveSeqs = Int.max(maxActiveSeqs, activeSeqs)
           , maxKeptSeqs = Int.max(maxKeptSeqs, keptSeqs)
           , maxActiveRules = Int.max(maxActiveRules, activeRules)
           , maxKeptRules = Int.max(maxKeptRules, keptRules)
           }

      fun pp (T { maxIterations, totalIterations, maxActiveSeqs, maxKeptSeqs
                , maxActiveRules, maxKeptRules }) =
         &[ %[$"max iterations   : ",PP.int maxIterations]
          , %[$"total iterations : ",PP.int totalIterations]
          , %[$"max active seqs  : ",PP.int maxActiveSeqs]
          , %[$"max kept seqs    : ",PP.int maxKeptSeqs]
          , %[$"max active rules : ",PP.int maxActiveRules]
          , %[$"max kept rules   : ",PP.int maxKeptRules]
          ]
   end

   datatype selection_policy = ExhaustSeqs | ExhaustRules

   datatype t = T of
      { goal: Seq.t
      , global: Seq.Ants.t
      (* databases *)
      , asd: ASD.t
      , ksd: KSD.t
      , ard: ARD.t
      , krd: KRD.t
      , pdb: PDB.t
      , foundGoal : bool ref
      , selectionPolicy: selection_policy
      , swapInterval: int
      , conflicts : Conflicts.t
      (* statistics *)
      , iters: int ref
      }
   type printable = t

   fun stats (T {asd, ksd, ard, krd, iters, ...}) =
      Stats.T1
         { iterations = !iters
         , activeSeqs = ASD.size asd
         , keptSeqs = KSD.size ksd
         , activeRules = ARD.size ard
         , keptRules = KRD.size krd
         }

   val ppShort = Stats.pp1 o stats

   fun pp (db as T {ksd, asd, ard, krd, ...}) =
      let
         val n = !Parameters.Db.printLength
         val n'= SOME n
      in
         if n = 0 then ppShort db else
         &[ ppShort db
          , %[\\, &[ ~
                   , ASD.pp asd n'
                   , KSD.pp ksd n'
                   , ARD.pp ard n'
                   , KRD.pp krd n'
                   ]]
          , ~
          , Util.line
          ]
      end

   (* Return the total number of removed rules and sequents. *)
   val doBackwardSubsumption : t * Seq.t -> int =
      fn (T {asd, ard, ksd, krd, ...}, seq) =>
         let
            val back = !Parameters.Db.useBackwardSubsumption
            val rule = !Parameters.Db.useRuleSubsumption
            val aqs = if back then ASD.subsumed (asd, seq) else Id.Set.empty
            val kqs = if back then KSD.subsumed (ksd, seq) else Id.Set.empty
            val ars =
               if back andalso rule then ARD.subsumed (ard, seq)
               else RuleId.Set.empty
            val krs =
               if back andalso rule then KRD.subsumed (krd, seq)
               else RuleId.Set.empty
         in
            ASD.remove (asd, aqs)
          ; KSD.remove (ksd, kqs)
          ; ARD.remove (ard, ars)
          ; KRD.remove (krd, krs)
          ; Id.Set.size aqs + RuleId.Set.size ars
               + Id.Set.size kqs + RuleId.Set.size krs
         end

   fun addKeptSeq (T {asd, ksd, goal, foundGoal, conflicts, ...}, seq, node:Node.t) =
      if not (Conflicts.allowed (conflicts, Seq.preds seq)) then
         Log.debug (fn () => %[$"Rejecting conflicting seq", Seq.pp seq])
      else if !Parameters.Db.useForwardSubsumption andalso
         (ASD.subsumes (asd, seq) orelse KSD.subsumes (ksd, seq))
      then Log.debug (fn () => %[$"Rejecting subsumed seq", Seq.pp seq])
      else
         let in
            Log.debug (fn () => %[$"Adding seq", Seq.pp seq])
          ; KSD.insert (ksd, seq, node, Seq.prio {seq=seq, goal=goal})
          ; if Seq.subsumes (seq, goal) then
               let in
                  Log.info (fn () => $"Found the goal")
                ; foundGoal := true
               end
            else ()
         end

   fun addKeptRule (T {asd, ksd, krd, goal, ...}, rule, node) =
      let
         val _ = Log.debug (fn () => %[$"Adding rule", Rule.pp rule])
         val concl = Rule.concl rule
         val prio = Rule.prio (rule, {goal=goal})
      in
         (* If the conclusion of the rule is subsumed, don't add it.  This
            helps enormously! ~10X on some problems *)
         if !Parameters.Db.useForwardSubsumption
            andalso !Parameters.Db.useRuleSubsumption
            andalso (ASD.subsumes (asd, concl) orelse KSD.subsumes (ksd, concl))
         then
            Log.debug (fn () => %[$"Rejecting subsumed rule", Rule.pp rule])
         else
            KRD.insert (krd, rule, node, {prio=prio})
      end

   fun create { stable = C.Stable.T { formula, conflicts, ...}
              , foci = C.Foci.T {global, rules, goal, ...} } =
      let
         val _ = asserts (fn () => PFormula.propositional formula, "non-propositional formula")
         val goal = Seq.ofFocus goal
         val global = Seq.Ants.ofList (map Focus.Left.pred global)
         fun ffn (r as Focus.Rule.T { hyps, concl, proof, ...}, (qs, rs)) =
            case hyps of
               [] => ((Seq.ofFocus concl, proof) :: qs, rs)
             | _ => (qs, (Rule.ofFocus r, proof) :: rs)
         val (seqs : (Seq.t * Fragment.t) list, rules) = foldr ffn ([], []) rules
         val nseqs = List.length seqs
         val nrules = List.length rules
         val _ = Log.msg
                    (fn level =>
                       if Log.gte (level, Log.Info) then
                          let
                             val goal =
                                &[ $"Goal"
                                 , %[\\, Seq.ppNoId goal] ]
                             val seqs =
                                &[ %[$"Initial seqs: ", PP.int nseqs]
                                 , if nseqs = 0 then ~ else
                                  %[\\, &(map (Seq.ppNoId o fst) seqs)]]
                             val rules =
                                &[ %[$"Initial rules: ", PP.int nrules]
                                 , if nrules = 0 then ~ else
                                  %[\\, &(List.separate \ (map (Rule.pp o fst) rules))]]
                             val global =
                                &[ $"Globals:"
                                 , ~
                                 , %[\\, Seq.Ants.pp global] ]
                             val res =
                                &[ Util.line
                                 , goal
                                 , seqs
                                 , rules
                                 , global
                                 , Util.line ]
                          in
                             SOME res
                          end
                       else if Log.gte (level, Log.Info) then
                          SOME (&[ %[$"Initial seqs : ", PP.int nseqs]
                                 , %[$"Initial rules: ", PP.int nrules] ])
                       else NONE)
         val global = if !Parameters.Focus.useGlobalization then global else Seq.Ants.empty
         val conflicts =
            if !Parameters.Db.useConflicts then conflicts
            else Conflicts.dummy
         val asd = ASD.create ()
         val ksd = KSD.create ()
         val ard = ARD.create ()
         val krd = KRD.create ()
         val pdb = PDB.create ()
         (* Start at 1 so a mod check doesn't succeed on 0 *)
         val iters = ref 1
         val sel = if !Parameters.Partial.exhaustSeqs then ExhaustSeqs else ExhaustRules
         val swap = !Parameters.Partial.swapInterval
         val db = T { goal = goal
                    , global = global
                    , asd = asd
                    , ksd = ksd
                    , ard = ard
                    , krd = krd
                    , pdb = pdb
                    , conflicts = conflicts
                    , foundGoal = ref false
                    , selectionPolicy = sel
                    , swapInterval = swap
                    , iters = iters
                    }
      in
         let in
            app (fn (seq, node) => addKeptSeq (db, seq, Node.Initial node)) seqs
          ; app (fn (rule, node) => addKeptRule (db, rule, Node.Initial node)) rules
         end
       ; db
      end

   (* Select either a rule from keptRules or a sequent from keptSeqs *)
   datatype phase = Rule | Seq | Done

   (* Decide on the policy for selecting a kept rule or sequent *)
   fun phase (T {foundGoal, krd, ksd, iters, selectionPolicy, swapInterval, ...}) =
      if !foundGoal then Seq else
      if KRD.isEmpty krd andalso KSD.isEmpty ksd then Done else
      case selectionPolicy of
         ExhaustSeqs =>
         if (!iters mod swapInterval = 0
             andalso not (KRD.isEmpty krd))
            orelse KSD.isEmpty ksd
         then Rule
         else Seq
       | ExhaustRules =>
         if (!iters mod swapInterval = 0
             andalso not (KSD.isEmpty ksd))
            orelse KRD.isEmpty krd
         then Seq
         else Rule

   (* Step the database. *)
   fun step (db as T { goal, ksd, asd, ard, krd, pdb, iters, ...}) =
      let in
         Ref.incr iters
       ; case phase db of
            Done => S.Saturated
          | Seq =>
            let
               (* select a kept sequent *)
               val (seq, node) = KSD.next ksd
               val qid = Seq.id seq
               val _ = Log.debug (fn () => %[$"Selected seq", Id.pp qid])
            in
               PDB.seq (pdb, qid, node)
             ; if Seq.subsumes (seq, goal) then
                  let
                     val pf =
                        if !Parameters.Proof.reconstruct then
                           let in
                              Log.debug (fn () => &[ $"Proof db:"
                                                   , %[\\, PDB.pp pdb]] )
                            ; SOME (PDB.reconstruct (pdb, qid))
                           end
                        else NONE
                  in
                     S.Proved pf
                  end
               else
                  let
                     (* CR: I should use this to update the priority of the sequent. *)
                     val _ = doBackwardSubsumption (db, seq)
                     (* add it to the active set *)
                     val () = ASD.insert (asd, seq)
                     (* generate new sequents and rules by matching q against
                        all rules in the active rule database. *)
                     val possRules = ARD.matches(ard, seq)
                     fun generate (rid, acc as (newQ, newR)) =
                        let
                           val rule = ARD.find (ard, rid)
                           val match = Rule.match (rule, seq)
                           open Rule.Match
                        in
                           case match of
                              NoMatch => acc
                            | NewSeqs qs =>
                              let
                                 val class = Node.Derived (rid, qid)
                                 val qs = map (fn q => (q, class)) qs
                              in
                                 (qs @ newQ, newR)
                              end
                            | NewRules rs =>
                              let
                                 val class = Node.Derived(rid, qid)
                                 val rs = map (fn r => (r, class)) rs
                              in
                                 (newQ, rs @ newR)
                              end
                        end
                     val (newSeqs, newRules) = RuleId.Set.foldl generate ([], []) possRules
                     (* add the new seqs and rules to the kept databases *)
                     val _ = Log.debug (fn () => %[$"Adding kept seqs", PP.int (length newSeqs)])
                     val () = app (fn (q, c) => addKeptSeq(db, q, c)) newSeqs
                     val _ = Log.debug (fn () => %[$"Adding kept rules", PP.int (length newRules)])
                     val () = app (fn (r, c) => addKeptRule(db, r, c)) newRules
                  in
                     S.Inconclusive
                  end
            end
          | Rule =>
            let
               (* select a kept rule *)
               val (rule, node) = KRD.next krd
               val rid = Rule.id rule
               val _ = PDB.rule (pdb, rid, node)
               val _ = Log.debug (fn () => %[$"Selected rule", RuleId.pp rid])
               (* add it to the active set *)
               val () = ARD.insert (ard, rule)
               (* generate new sequents and rules by matching rule against
                  all sequents in the active sequent database *)
               val possSeqs = ASD.matches (asd, rule)
               (* val _ = printl ("num matches: " ^ Int.toString (Seq.Id.Set.size possSeqs)) *)
               fun generate (qid, acc as (newQ, newR)) =
                  let
                     val seq = ASD.find (asd, qid)
                     val match = Rule.match (rule, seq)
                     val _ = Log.debug (fn () =>
                        &[ %[$"Rule", Rule.pp rule]
                         , %[$"Seq", Seq.pp seq]
                         , %[$"Match", Rule.Match.pp match]
                         ])
                     open Rule.Match
                  in
                     case match of
                        NoMatch => acc
                      | NewSeqs seqs =>
                        let
                           val class = Node.Derived (rid, qid)
                           val seqs = map (fn q => (q, class)) seqs
                        in
                           (seqs @ newQ, newR)
                        end
                      | NewRules rs =>
                        let
                           val class = Node.Derived (rid, qid)
                           val rs = map (fn r => (r, class)) rs
                        in
                           (newQ, rs @ newR)
                        end
                  end
               val (newSeqs, newRules) = Id.Set.foldl generate ([], []) possSeqs
               (* add the new sequents and rules to the kept databases *)
               val _ = Log.debug (fn () => %[$"Adding kept seqs", PP.int (length newSeqs)])
               val () = app (fn (q, c) => addKeptSeq(db, q, c)) newSeqs
               val _ = Log.debug (fn () => %[$"Adding kept rules", PP.int (length newRules)])
               val () = app (fn (r, c) => addKeptRule(db, r, c)) newRules
            in
               S.Inconclusive
            end
      end

   val () = noWarnUnused (fn _ : printable => ())
end
