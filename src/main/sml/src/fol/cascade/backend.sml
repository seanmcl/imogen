
structure Backend :> Backend = struct
   structure C = Focus
   structure S = BackendUtil.Status

   open General
   open PP.Ops

   structure Stats = struct
      datatype one = One of
         { iterations: int
         , active: int
         , kept: int
         , rules: int }

      fun pp1 (One { iterations, active, kept, rules }) =
         %[ %[$"i: ",PP.int iterations]
          , %[$", as: ",PP.int active]
          , %[$", ks: ",PP.int kept]
          , %[$", rs: ",PP.int rules] ]

      datatype t = T of
         { maxIterations: int
         , totalIterations: int
         , maxActive: int
         , maxKept: int
         , maxRules: int }

      val empty =
         T { maxIterations = 0
           , totalIterations = 0
           , maxActive = 0
           , maxKept = 0
           , maxRules = 0 }

      fun add ( T { maxIterations, totalIterations, maxActive
                  , maxKept, maxRules }
              , One { iterations, active, kept, rules }) =
         T { maxIterations = Int.max (maxIterations, iterations)
           , totalIterations = iterations + totalIterations
           , maxActive = Int.max (maxActive, active)
           , maxKept = Int.max (maxKept, kept)
           , maxRules = Int.max (maxRules, rules) }

      fun pp (T { maxIterations, totalIterations, maxActive, maxKept
                , maxRules }) =
         &[ %[$"max iterations   : ",PP.int maxIterations]
          , %[$"total iterations : ",PP.int totalIterations]
          , %[$"max active seqs  : ",PP.int maxActive]
          , %[$"max kept seqs    : ",PP.int maxKept]
          , %[$"max rules        : ",PP.int maxRules]
          ]
   end

   (* structure Input = struct *)
   (*    datatype t = T of *)
   (*       { f : LFormula.neg *)
   (*       , goal: Seq.t *)
   (*       , preds: Pred.set *)
   (*       , seqs: (Seq.t * SC.t) list *)
   (*       , rules: RuleInput.t list *)
   (*       , conflicts : Conflicts.t *)
   (*       , global: Seq.Ants.t } *)
   (* end *)

   (* Info needed during a single proof of a stable sequent *)
   datatype t = T of
      { goal: Seq.t
      , global: Seq.Ants.t
      (* databases *)
      , rules: Rule.t
      , active: Active.t
      , kept: Kept.t
      , proof: Proof.t
      (* statistics *)
      , iters: int ref }
   type printable = t

   val stats : t -> Stats.one = fn
      (T {active, kept, rules, iters, ...}) =>
      Stats.One { iterations = !iters
                , active = Active.size active
                , kept = Kept.size kept
                , rules = Rule.size rules
                }

   val ppShort = Stats.pp1 o stats

   val line = $(String.implode (List.replicate (80, #"-")))

   fun pp (db as T { kept, active, rules, proof, ...}) =
      let
         val ksize = Kept.size kept
         val asize = Active.size active
         val k = if ksize > 0 then
                    &[ %[$"Kept: ", PP.paren (PP.int ksize)]
                     , %[\\, Kept.pp kept NONE] ]
                 else ~
         val a = if asize > 0 then
                    &[ %[$"Active: ", PP.paren (PP.int asize)]
                     , %[\\, Active.pp active NONE]]
                 else ~
         val p = ~
         (* val p = &[ \ *)
         (*          , $"Proof  : " *)
         (*          , %[\\, Proof.pp proof]] *)
         val () = noWarnUnused (proof)
      in
         &[ line
          , ~
          , &[ $"Database: "
             , %[\\, &[ ppShort db
                      , a
                      , k
                      , &(PP.punctuate \
                             [ %[$"Rules  : ", PP.paren (PP.int (Rule.size rules))]
                             , %[\\, Rule.pp rules] ])
                      , p
                      ]
                ]] ]
      end

   val addKeptSeq : t * Seq.t * int * Proof.Node.t -> unit = fn
      (T {active, kept, ...}, seq, prio, node) =>
      if !Parameters.Db.useForwardSubsumption andalso
         (Active.subsumes (active, seq) orelse Kept.subsumes (kept, seq))
      then Log.trace (fn () => %[$"Rejecting subsumed: ", Seq.ppNoId seq])
      else let in
         Log.trace (fn () => %[$"Adding seq: ", Seq.pp seq])
       (* Add it to kept *)
       ; Kept.insert (kept, seq, node, prio)
      end

   fun create { stable = C.Stable.T { lformula, conflicts, bipolarPreds, ...}
              , foci = C.Foci.T {global, rules, goal, ...} } =
      let
         val goal = Seq.ofFocus goal
         val global = Seq.Ants.ofList (map Focus.Left.label global)
         fun ffn (r as Focus.Rule.T { hyps, concl, proof, ...}, (qs, rs)) =
            case hyps of
               [] =>
               let
                  val proof = case proof of
                     Fragment.Leaf p => p
                   | _ => raise Impossible
                  val q = Seq.ofFocus concl
               in
                  ((q, proof) :: qs, rs)
               end
             | _ => (qs, RuleInput.ofFocus r :: rs)
         val (seqs, rules) = foldr ffn ([], []) rules
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
                                  %[\\, &(List.separate \ (map RuleInput.pp rules))]]
                             val global =
                                &[ $"Globals:"
                                 , ~
                                 , %[\\, Seq.Ants.pp global] ]
                             val res =
                                &[ Util.line
                                 , goal
                                 , \
                                 , seqs
                                 , \
                                 , rules
                                 , \
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
         (* Build the initial database *)
         val _ = Log.trace (fn () => $"creating initial db")
         val conflicts =
            if !Parameters.Db.useConflicts then conflicts
            else Conflicts.dummy
         val active = Active.create {global=global}
         val kept =
            Kept.create
               { global = global
               , depthInterval = !Parameters.Db.depthInterval }
         val proof = Proof.create {f = lformula, global = global}
         val initialSeqs = map fst seqs
         val rules = Rule.create
                        { proof = proof, goal = goal, initialSeqs = initialSeqs,
                          rules = rules, preds = bipolarPreds, conflicts = conflicts,
                          kept = kept, active = active, global = global }
         (* Start at 1 so a mod check doesn't succeed on 0 *)
         val iters = ref 1
         val db = T { goal = goal
                    , global = global
                    , active = active
                    , kept = kept
                    , rules = rules
                    , proof = proof
                    , iters = iters }
      in
         let in
            (* add initial sequents *)
            List.app
               (fn (seq, sc) =>
                   addKeptSeq ( db, seq, Seq.prio { seq = seq, goal = goal }
                              , Proof.Node.Initial sc))
               seqs
          ; db
         end
      end

   fun step (db as T { goal, global, proof, kept, active, rules, iters, ...}) =
      let in
         Ref.incr iters
       ; if Kept.isEmpty kept then S.Saturated else
         let
            val (seq, node) = Kept.next kept
            val _ = Log.info (fn () => %[ $"Selected: ", Seq.pp seq ])
            val _ = Proof.addSeq (proof, seq, node)
         in
            case Seq.subsumes' (seq, goal, {global=SOME global}) of
               SOME theta =>
               let
                  val pf =
                     if !Parameters.Proof.reconstruct then
                        let
                           val pf = Proof.reconstruct (proof, Seq.id seq)
                           val pf = SC.apply (pf, theta)
                        in
                           SOME pf
                        end
                     else NONE
               in
                  S.Proved pf
               end
             | NONE =>
               let
                  val () = Active.insert (active, seq)
                  val () =
                     if not (!Parameters.Db.useRuleSubsumption) then ()
                     else Rule.removeSubsumed (rules, seq)
                  val new = Rule.match (rules, seq)
               in
                  Log.trace (fn () => %[ $"Adding kept seqs: ", PP.int (List.length new)])
                ; List.app
                     (fn (Rule.Out { seq, prio, parents }) =>
                         addKeptSeq (db, seq, prio, Proof.Node.Derived parents))
                     new
                ; S.Inconclusive
               end
         end
      end

   val () = noWarnUnused (fn _ : printable => ())
end
