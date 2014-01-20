
structure Rule :> Rule = struct

   structure Ants = Seq.Ants
   structure Cons = Seq.Cons

   open General
   open PP.Ops
   open Cons.Ops

   type id = RuleId.t

   (* ----------------------------------------------------------------------- *)

   structure Input = struct
      datatype t = T of
         { id : id
         , hyp : Seq.t
         , hyps : Seq.t list
         , concl : Seq.t
         , proof : Fragment.t }

      fun id (T {id, ...}) = id

      fun ofFocus (Focus.Rule.T {hyps, concl, proof, ...}) =
         let
            val (hyp, hyps) = case map Seq.ofFocus hyps of
               [] => raise Impossible
             | h :: hs => (h, hs)
            val concl = Seq.ofFocus concl
         in
            T { id = RuleId.next ()
              , hyp = hyp
              , hyps = hyps
              , concl = concl
              , proof = proof }
         end

      fun pp (T { id, hyp, hyps, concl, ... }) =
         &[ PP.sep (PP.commas (map Seq.ppNoId (hyp :: hyps)))
          , %[$"---------------------------", RuleId.pp id]
          , Seq.ppNoId concl
          ]

      fun subsumes (T { hyp, hyps, concl, ...},
                     T { hyp=hyp2, hyps=hyps2, concl=concl2, ...}) =
         length hyps <= length hyps2 andalso
         Seq.subsumes (concl, concl2) andalso
         List.all
            (fn q1 => List.exists
                         (fn q2 =>
                            Ants.subset (Seq.ants q2, Seq.ants q1)
                            andalso Cons.eq (Seq.cons q1, Seq.cons q2))
                         (hyp2 :: hyps2)) (hyp :: hyps)

      fun eq ( T { hyp, hyps, concl, ...},
               T { hyp=hyp2, hyps=hyps2, concl=concl2, ...} ) =
         ListPair.allEq Seq.eq (concl :: hyp :: hyps, concl2 :: hyp2 :: hyps2)
   end

   (* ----------------------------------------------------------------------- *)

   type out = Id.t list Index.t

   structure Branch : sig
      type t
      val hyps : t -> Seq.t list
      val concl : t -> Seq.t
      val pp : {exact : bool} -> t -> PP.t
      val size : t -> int
      val create : {hyp: Seq.t, hyps: Seq.t list, concl:Seq.t, initial: bool} -> t
      val removeSubsumed : t * Seq.t -> unit
      val apply
         : { t: t, kept: Kept.t, active: Active.t, conflicts: Conflicts.t
           , seq: Seq.t, exact: bool}
         * out -> out
   end = struct

      datatype t = T of
         { id : id
         , initial : bool
         , hyp : Seq.t
         , hyps : Seq.t list
         , concl : Seq.t
         , active : Id.t list Index.t
         , kept : Id.t Index.t
         , matchable : bool ref }

      type printable = t

      fun hyps (T { hyp, hyps, ... }) = hyp :: hyps
      fun concl (T { concl, ... }) = concl

      fun create {hyp, hyps, concl, initial} =
         let
            val active = Index.create ()
         in
            if initial then
               Index.insert (active, Seq.id concl, concl, [])
            else ()
          ; T { id = RuleId.next ()
              , initial = initial
              , hyp = hyp
              , hyps = hyps
              , concl = concl
              , active = active
              , kept = Index.create ()
              , matchable = ref true }
         end

      fun size (T { active, kept, ... }) =
         Index.size active + Index.size kept

      fun pp {exact} (T { id, initial, hyp, hyps, concl, active, kept
                        , matchable } : printable) =
         let
            val an = PP.paren (PP.int (Index.size active))
            val kn = PP.paren (PP.int (Index.size kept))
            val a = if Index.isEmpty active then ~ else
               %[ $"a: ", &(map Seq.ppNoId (Index.listSeqs active))]
            val k = if Index.isEmpty kept then ~ else
               %[ $"k: ", &(map Seq.ppNoId (Index.listSeqs kept))]
            val initial = if initial then $"*" else ~
            val matchable = if !matchable then ~ else $"X"
            val exact = if exact then $"!" else ~
         in
            &[ PP.sep (PP.commas (map Seq.ppNoId (hyp :: hyps)))
             , %[$"---------------------------",RuleId.pp id, initial
                , matchable, \, exact, an, $":", kn ]
             , Seq.ppNoId concl
             , a
             , k ]
         end

      fun removeSubsumed (T { active, kept, ... }, seq) =
         let
            fun remove ind =
               Id.Set.app
                  (fn id => Index.remove (ind, id))
                  (Index.subsumed (ind, seq))
         in
            remove active
          ; remove kept
         end

      val match1 : Kept.t * Active.t * Conflicts.t * bool * Seq.t *
         Seq.t * Seq.t -> Seq.t option =
         fn (kept, active, conflicts, exact, seq, hyp, concl) =>
            let
               val q_ants = Seq.ants seq
               val q_cons = Seq.cons seq
               val r_ants = Seq.ants hyp
               val r_cons = Seq.cons hyp
               (* matchCons (seq_cons, rule_cons) --> SOME (strict, inst)
                  [strict] means one of the antecedents must match.
                  [inst] means the conclusion consequent is instantiated. *)
               fun matchCons exact = fn
                  (P l1, P l2) =>
                  if Pred.eq (l1, l2) then SOME (false, false) else NONE
                | (Xi, _) => SOME (true, false)
                | (_, Xi) => if exact then NONE else SOME (true, true)
            in
               case matchCons exact (q_cons, r_cons) of
                  NONE =>
                  let in
                     (* Log.trace (fn () => $"consequents differ"); *)
                     NONE
                  end
                | SOME (strict, inst) =>
                  if strict andalso
                     Ants.isEmpty (Ants.intersection (q_ants, r_ants)) then
                     let in
                        (* Log.trace (fn () => $"no common antecedents for strict match"); *)
                        NONE
                     end
                  else let
                     val antsd = Ants.difference (q_ants, r_ants)
                  in
                     if exact andalso not (Ants.isEmpty antsd) then
                        let in
                           Log.trace (fn () => $"Rejecting inexact match")
                         ; NONE
                        end
                     else let
                        val ants = Ants.union (Seq.ants concl, antsd)
                        val cons = if inst then q_cons else Seq.cons concl
                        val seq = Seq.new (ants, cons)
                     in
                        if not (Conflicts.allowed (conflicts, Seq.preds seq)) then
                           let in
                              Log.trace
                                 (fn () => %[$"Rejecting conflicting sequent: ",
                                              Seq.ppNoId seq])
                            ; NONE
                           end
                        else if
                           Kept.subsumes (kept, seq)
                           orelse Active.subsumes (active, seq)
                        then
                           let in
                              Log.trace (fn () => %[ $"Rejecting subsumed match: "
                                                   , Seq.ppNoId seq])
                            ; NONE
                           end
                        else SOME seq
                     end
                  end
            end

      fun apply ({t = T { id, initial, hyp, concl, active, kept, matchable, ... },
                   kept = ksd, active = asd, conflicts, seq, exact }, new) =
         let
            val out = Index.create ()
            val q_id = Seq.id seq
            val _ = Log.trace
                       (fn () => %[$"Matching: r", RuleId.pp id
                                  , $", q", Id.pp (Seq.id seq)])
            val k =
               if !matchable then
                  match1 (ksd, asd, conflicts, exact, seq, hyp, concl)
               else NONE
            fun add (k, q, ps) = case Seq.combine (k, q) of
               NONE => ()
             | SOME q => Index.insertRemoveSubsumed (out, Seq.id q, q, ps)
         in
            case k of
               NONE => ()
             | SOME k =>
               let in
                  (* out += k <+> active *)
                  Index.app (fn (q, ps) => add (k, q, q_id :: ps)) active
                (* out += k <+> new *)
                ; Index.app (fn (q, ps) => add (k, q, q_id :: ps)) new
               end
          ; (* out += new <+> kept *)
            Index.app
               (fn (n, ps) =>
                   Index.app
                      (fn (k, p) => add (n, k, p :: ps)) kept) new
          (* active += new *)
          ; if !matchable then
               Index.app
                  (fn (q, ps) =>
                      if not (Index.subsumes (active, q)) then
                         let in
                            Index.removeSubsumed (active, q)
                          ; Index.removeSubsumed (kept, q)
                          ; Index.insert (active, Seq.id q, q, ps)
                         end
                      else ())
                  new
            else ()
          (* If the match was exact, disable the branch. *)
          ; case k of
               NONE => ()
             | SOME s =>
               if !Parameters.Cascade.useDisableBranchOnExactMatch
                  andalso Seq.subsumes (s, concl)
               then
                  let in
                     Index.clear kept
                   ; Index.clear active
                   ; matchable := false
                  end
               else ()
          (* kept += k *)
          ; if initial then () else
            case k of
               NONE => ()
             | SOME k =>
               let in
                  if Seq.subsumes (seq, k) orelse Index.subsumes (active, k)
                  then ()
                  else Index.insertRemoveSubsumed (kept, Seq.id k, k, q_id)
               end
          ; out
         end
   end

   (*------------------------------------------------------------------------ *)

   structure Rule = struct
      datatype t = T of
         { id : RuleId.t
         , init : Branch.t
         , rest : Branch.t list
         , exact : bool ref }

      fun unboundCons (T { init, ... }) =
         Cons.isXi (Seq.cons (Branch.concl init)) andalso
         List.exists (Cons.isXi o Seq.cons) (Branch.hyps init)

      fun hyps (T { init, ... }) = Branch.hyps init

      fun concl (T { init, ...}) = Branch.concl init

      fun id (T {id, ...}) = id

      fun pp (T {init, rest, exact = ref exact, ...}) =
         &(List.separate \ (map (Branch.pp {exact=exact}) (init :: rest)))

      fun size (T { init, rest, ... }) =
         List.foldl (fn (x, n) => Branch.size x + n)
            (Branch.size init) rest

      fun removeSubsumed (T { init, rest, ... }, seq) =
         List.app (fn b => Branch.removeSubsumed (b, seq)) (init :: rest)

      val match : (t * Kept.t * Active.t * Conflicts.t * Proof.t
                    * Seq.t) * out -> out =
         fn ((T {init, rest, exact = ref exact, ...}, ksd, asd
             , conflicts, _, seq), new) =>
            let
               fun fold (b, out) =
                  Branch.apply ({ t = b, kept = ksd, active = asd
                                , conflicts = conflicts, seq = seq
                                , exact = exact}, out)
            in
               List.foldl fold new (init :: rest)
            end

      val create : Proof.t * Input.t -> t = fn
         (pdb, Input.T { id, hyp, hyps, concl, proof, ... }) =>
         let
            val init = Branch.create { hyp=hyp, hyps=hyps
                                     , concl=concl, initial=true}
            fun aux (hyp, (acc, hyps)) =
               let
                  val hyps = case hyps of
                     [] => raise Impossible
                   | _ :: hyps => hyps
                  val b = Branch.create { hyp=hyp, hyps=hyps
                                        , concl=concl, initial=false }
               in
                  (b :: acc, hyps)
               end
            val (rest, _) = List.foldl aux ([], hyps) hyps
         in
            Proof.rule (pdb, id, proof)
          ; T { id = id
              , init = init
              , rest = rev rest
              , exact = ref false }
         end
   end

   (* ----------------------------------------------------------------------- *)

   datatype t = T of
      { rules : Rule.t list ref
      , conclIndex : unit Index.t
      , hypIndex : unit Index.t
      , goal : Seq.t
      , initialSeqs : Seq.t list
      , preds : Pred.set
      , kept : Kept.t
      , active : Active.t
      , conflicts : Conflicts.t
      , proof : Proof.t }

   fun prio (T { conclIndex, hypIndex, goal, ... }, seq) =
      Seq.prio { seq = seq, goal = goal } +
         (if not (Id.Set.isEmpty (Index.subsumed (conclIndex, seq))) then 400000
          else if not (Id.Set.isEmpty (Index.subsumed (hypIndex, seq))) then 100000
          else 0)

   fun size (T { rules = ref rs, ... }) =
      foldl (fn (r, n) => Rule.size r + n) 0 rs

   fun pp (T {rules = ref rs, hypIndex=_, conclIndex=_, ...}) =
      &[ $"Rules"
       , %[\\, &(List.separate \ (map Rule.pp rs))]
       (* , $"conclIndex" *)
       (* , %[\\, Index.pp (fn _ => ~) conclIndex ] *)
       (* , $"hypIndex" *)
       (* , %[\\, Index.pp (fn _ => ~) hypIndex ] *)
       ]

   fun removeSubsumed (T { rules as ref l, conclIndex, hypIndex, ...}, seq) =
      let
         val (obs, ok) =
            List.partition (fn r => Seq.subsumes (seq, Rule.concl r)) l
         fun f (Rule.T { init, ... }) =
            let in
               Index.remove (conclIndex, Seq.id (Branch.concl init))
             ; List.app (fn h => Index.remove (hypIndex, Seq.id h)) (Branch.hyps init)
            end
      in
         List.app f obs
       ; rules := ok
       ; List.app (fn b => Rule.removeSubsumed (b, seq)) ok
      end

   (* A rule
     H1  ...  HN
     -----------
       Γ ⊢ P
     must match exactly (P is not Ξ) if there is no hyp of the form Δ ⊢ P
     (including H1...HN).  (I.e. P is only a conclusion.)  If this is the case,
     any sequent of the form Δ ⊢ P can never have hypotheses removed.

     You might think this is not very strong, as any hyp of the form Δ ⊢ Ξ would
     seem to block the optimization.  However, if Δ ⊢ Ξ is a hyp, then the
     concl is Δ' ⊢ Ξ, so you still can't remove any.
     Still, it is occasionally useful (e.g. the SYJ201 family of ILTP).

     If Γ is nonempty, we can throw this rule out completely, but I haven't
     implemented this yet.

     A rule

     H1 ... HN
     ---------
      · ⊢ g

     is NOT exact if g is the goal and there are no other rules with
     conclusion · ⊢ g.  I keep making this mistake.  You can have an
     inexact match Γ ⊢ g, then use ⊢ g in a hyp to move the antcedents to,
     say, Γ ⊢ a, then remove the antecedents with another rule, say to
     ⊢ a = Hi.

     It's true that it must be the last rule applied in a backward proof, but
     you can't assume it's used exactly once.
    *)
   fun setExact (T { rules = ref rules, hypIndex, ... }) =
      let
         fun exact1 (Rule.T { init, exact, ...}) =
            case Seq.cons (Branch.concl init) of
               Xi => ()
             | cons =>
               if not (Id.Set.isEmpty (Index.undefinedCons hypIndex)) then ()
               else let
                  (* ⊢ p subsumes any hyp of the form Γ ⊢ p *)
                  val ps = Index.subsumed (hypIndex, Seq.new (Ants.empty, cons))
               in
                  if Id.Set.isEmpty ps then exact := true else ()
               end
      in
         Log.trace (fn () => $"Finding exact matches.")
       ; List.app exact1 rules
       ; Log.trace (fn () => $"Exact matches complete.")
      end

   (* Remove

      - rules of the form

          ... Γ ⊢ A ...
         ---------------
             Γ' ⊢ B

        where A is not a cons of an initial sequent and not the conclusion of
        any rule.  Ξ complicates things somewhat.  If some other rule has the
        form

            H1 ... HN
         ---------------
             Γ' ⊢ Ξ

        where no Ξ occurs in H1..HN, then it would be possible to match
        Γ ⊢ A with the result.  However, if Hi = Γ'' ⊢ A, then we can't
        really generate A as a concl from this rule, since we would have
        to have A as a concl already.  Thus we should ignore such rules
        when determining if A can be a conclusion. Moreover, if
        Hi = Γ'' ⊢ Ξ then we'd also need to have A as a concl already.

      - rules of the form

          ...  Γ, A ⊢ B ...
          -----------------
                Γ' ⊢ B

        where A does not occur in the antecedents of any initial sequent or
        conclusion.  For any sequent that matches that hyp will subsume
        the goal.  In this case, it's fine if B is Ξ.
   *)

   fun pruneUnmatchableRules (T { rules, goal, hypIndex, conclIndex, initialSeqs, ... }) =
      let
         val _ = Log.info (fn () => $"Pruning unmatchable rules")
         val hasInitialXi = List.exists Cons.isXi (List.map Seq.cons initialSeqs)
         fun loop () =
            let
               fun pred q = case Seq.cons q of Cons.P p => SOME p | Cons.Xi => NONE
               val initial_goals = Pred.Set.ofList (List.mapPartial pred initialSeqs)
               val concls = goal :: initialSeqs @ Index.listSeqs conclIndex
               val concl_ants = foldl
                                   (fn (q, s) => Ants.union (Seq.ants q, s)) Ants.empty concls
               fun unmatchableAnts concl hyp =
                  Cons.eq (Seq.cons concl, Seq.cons hyp) andalso
                  Ants.all (fn p => not (Ants.mem (concl_ants, p))) (Seq.ants hyp)
               fun unmatchableCons rules hyp =
                  not hasInitialXi andalso
                  case Seq.cons hyp of
                     Xi => false
                   | cons as P p =>
                     not (Pred.Set.mem (initial_goals, p)) andalso
                     let
                        fun hasP r = List.exists
                                        (fn rhyp => Cons.eq (Seq.cons rhyp, cons)) (Rule.hyps r)
                        val rules = List.filter (not o Rule.unboundCons) rules
                        val rules = List.filter (not o hasP) rules
                        fun conclP r =
                           let
                              val rcons = Seq.cons (Rule.concl r)
                           in
                              Cons.isXi rcons orelse Cons.eq (cons, rcons)
                           end
                        val rules = List.filter conclP rules
                     in
                        List.null rules
                     end
               fun unmatchable rules r =
                  let
                     val concl = Rule.concl r
                  in
                     List.exists
                        (fn hyp => unmatchableCons rules hyp
                            orelse unmatchableAnts concl hyp)
                        (Rule.hyps r)
                  end
               val (obs, ok) = List.partition (unmatchable (!rules)) (!rules)
               fun remove (Rule.T { id, init, ...}) =
                  let in
                     Log.info
                        (fn () => %[$"Pruning useless rule: ", RuleId.pp id])
                   ; Index.remove (conclIndex, Seq.id (Branch.concl init))
                   ; List.app
                        (fn q => Index.remove (hypIndex, Seq.id q))
                        (Branch.hyps init)
                  end
            in
               List.app remove obs
             ; rules := ok
             ; if List.length obs > 0 then loop () else ()
            end
      in
         loop ()
       ; Log.info (fn () => $"Pruning complete.")
      end

   fun create { proof, goal, initialSeqs, preds, rules, conflicts, kept, active } =
      let
         val rs =
            List.foldr
               (fn (t, acc) =>
                   if List.exists (fn t' => Input.subsumes (t', t)) acc then
                      let in
                         Log.info (fn () =>
                            %[ $"Ignoring redundant rule: "
                             , RuleId.pp (Input.id t)])
                       ; acc
                      end
                   else
                      t ::
                      List.filter
                         (fn t' =>
                            if Input.subsumes (t, t') then
                               let in
                                  Log.info (fn () =>
                                     %[ $"Ignoring redundant rule: "
                                      , RuleId.pp (Input.id t')])
                                ; false
                               end
                            else true) acc)
               [] rules
         val rules = List.map (fn r => Rule.create (proof, r)) rs
         val hypIndex = Index.create ()
         val conclIndex = Index.create ()
         fun f (Rule.T { init, ... }) =
            let
               val concl = Branch.concl init
            in
               Index.insert (conclIndex, Seq.id concl, concl, ())
             ; List.app
                  (fn hyp => Index.insert (hypIndex, Seq.id hyp, hyp, ()))
                  (Branch.hyps init)
            end
         val _ = List.app f rules
         val conflicts = Conflicts.restrict (conflicts, preds)
         val _ = Log.info (fn () => Conflicts.pp conflicts)
         val t = T
                    { rules = ref rules
                    , conclIndex = conclIndex
                    , hypIndex = hypIndex
                    , goal = goal
                    , initialSeqs = initialSeqs
                    , preds = preds
                    , conflicts = conflicts
                    , kept = kept
                    , active = active
                    , proof = proof }
      in
         if !Parameters.Cascade.useExactMatches then setExact t else ()
       ; if !Parameters.Cascade.usePruning then pruneUnmatchableRules t else ()
       ; t
      end

   datatype out = Out of
      { seq : Seq.t
      , prio : int
      , parents : id * Id.t list }

   fun match ( t as T { rules = ref rs, conflicts, kept, active, proof, ... }
             , seq) =
      let
         fun f (r, rest) =
            let
               val id = Rule.id r
               val new = Index.create ()
               val out =
                  Rule.match
                     ((r, kept, active, conflicts, proof, seq), new)
               val out = Index.toListi out
               val out = List.map (fn (q, ids) => (q, (id, rev ids))) out
            in
               out @ rest
            end
         val out = List.foldl f [] rs
         fun g (seq, parents) =
            let
               val prio = prio (t, seq)
            in
               Out { seq = seq
                   , prio = prio
                   , parents = parents }
            end
      in
         Log.trace (fn () => &[ %[$"Matched: ", PP.paren (PP.int (length out))]
                              , %[\\, &(map (Seq.ppNoId o fst) out)]])
       ; List.map g out
      end

   val () = noWarnUnused (Input.eq)

end
