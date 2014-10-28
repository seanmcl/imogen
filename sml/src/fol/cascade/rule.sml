
structure Rule :> Rule = struct
   structure Cons = Seq.Cons
   structure PSet = Param.Set
   structure VSet = Var.Set
   structure Input = RuleInput

   open General
   open PP.Ops
   open Cons.Ops

   type id = RuleId.t

   (* ----------------------------------------------------------------------- *)

   structure Out = struct
      type one = (Id.t * Subst.t) list
      type t = one RIndex.t
      fun pp1 (id, t) = %[PP.pair (Id.pp id, Subst.pp t)]
      fun pp l = PP.list (map pp1 l)
   end

   structure Branch : sig
      type t
      val hyps : t -> Seq.t list
      val concl : t -> Seq.t
      val pp : t -> PP.t
      val size : t -> int
      val create
         : { hyp: Seq.t, hyps: Seq.t list, concl:Seq.t
           , fresh: Param.set, initial: bool, global:Seq.Ants.t
           , ruleAtoms: Atoms.t } -> t
      val removeSubsumed : t * Seq.t -> unit
      val apply
         : { t : t, kept: Kept.t, active: Active.t, conflicts : Conflicts.t
           , seq: Seq.t}
         * Out.t -> Out.t
   end = struct

      (* Note: we don't really need hyps here, but it's nice to have
         them for printing. *)
      datatype t = T of
         { id : id
         , initial : bool
         , hyp : Seq.t
         , hyps : Seq.t list
         , concl : Seq.t
         , fresh : Param.set
         , active : (Id.t * Subst.t) list RIndex.t
         , kept : (Id.t * Subst.t) RIndex.t
         , global : Seq.Ants.t
         , ruleAtoms : Atoms.t }

      fun hyps (T { hyp, hyps, ... }) = hyp :: hyps

      fun concl (T { concl, ... }) = concl

      fun create {hyp, hyps, concl, fresh, initial, global, ruleAtoms} =
         let
            val active = RIndex.create {global=global, atoms = ruleAtoms, pp = Out.pp}
            val kept = RIndex.create {global = global, atoms = ruleAtoms, pp = Out.pp1}
            val rconcl = RSeq.ofSeq concl
         in
            (* The first branch begins with an active concl. *)
            if initial then
               RIndex.insert (active, rconcl, [])
            else ()
          ; T { id = RuleId.next ()
              , initial = initial
              , hyp = hyp
              , hyps = hyps
              , concl = concl
              , fresh = fresh
              , active = active
              , kept = kept
              , global = global
              , ruleAtoms = ruleAtoms}
         end

      fun size (T { active, kept, ... }) = RIndex.size active + RIndex.size kept

      fun pp (T { id, initial, hyp, hyps, concl, fresh, active, kept, ... }) =
         let
            val an = PP.int (RIndex.size active)
            val kn = PP.int (RIndex.size kept)
            val a = if RIndex.isEmpty active then ~ else
               %[ $"a: ", &(map (RSeq.pp) (RIndex.listSeqs active))]
            (* %[ $"a: ", &(map (RSeq.pp' atoms) (RIndex.listSeqs active))] *)
            val k = if RIndex.isEmpty kept then ~ else
               %[ $"k: ", &(map (RSeq.pp) (RIndex.listSeqs kept))]
            (* %[ $"k: ", &(map (RSeq.pp' atoms) (RIndex.listSeqs kept))] *)
            val initial = if initial then $"* " else ~
         in
            &[ PP.sep (PP.commas (map Seq.ppNoId (hyp :: hyps)))
             , %[ $"---------------------------", Param.Set.pp fresh, \,
                 RuleId.pp id, initial, $"(a: ", an, $", k: ", kn, $")" ]
             , Seq.ppNoId concl
             , a
             , k ]
         end

      fun removeSubsumed (T { active, kept, ... }, seq) =
         let
            val rseq = RSeq.ofSeq seq
            fun remove ind = RIndex.removeSubsumed (ind, rseq)
         in
            remove active
          ; remove kept
         end

      fun match1 ( kept, active, conflicts, seq, hyp, fresh, concl,
                  global, ruleAtoms )
         : { rseq : RSeq.t, renaming : Subst.t} list =
         let
            (* Rename the input sequent, since we may want to match the same
               sequent to multiple branches, where each match corresponds to
               a different substitution instance of the sequent. *)
            val oldseq = seq
            val (seq, renaming) = Seq.rename (seq, Subst.id)
            val _ = Log.trace (fn () =>
               &[ $"Renamed "
                , %[\\, Seq.ppNoId oldseq]
                , $"to"
                , %[\\, Seq.ppNoId seq]
                , $"with "
                , %[\\, Subst.pp renaming]])
            val _ = Subst.invariant renaming
            val _ =
               assert
                  (fn () =>
                     Atoms.eq (Seq.atoms seq, Subst.img renaming),
                   fn () => &[ $"Rule.match1"
                             , %[\\, &[ Seq.pp seq
                                      , Subst.pp renaming ]]])
            val subs = Seq.match { seq = seq, hyp = hyp
                                 , concl = concl, fresh = fresh
                                 , global = global
                                 , atoms = ruleAtoms }
            fun mfn {concl=seq, theta=unifier, filledHole=_} =
               let in
                  Subst.invariant unifier
                ; if not (Conflicts.allowed (conflicts, Seq.preds seq))
                  then let in
                     Log.trace (fn () => %[ $"Rejecting conflicting sequent: ",
                                           Seq.ppNoId seq ])
                   ; NONE
                  end
                  else if
                     Kept.subsumes (kept, seq)
                     orelse
                     Active.subsumes (active, seq)
                  then let in
                     Log.trace (fn () => %[ $"Rejecting subsumed match: ",
                                           Seq.ppNoId seq ])
                   ; NONE
                  end
                  else SOME { rseq = RSeq.make (seq, unifier)
                            , renaming = renaming }
               end
            val res = List.mapPartial mfn subs
            fun ppres {rseq, renaming} = %[RSeq.pp rseq, $" : renaming = ", Subst.pp renaming]
            val _ =
               Log.trace
                  (fn () =>
                     %[\\, $"Rule.match1: ",
                       &[ %[$"seq    : ", Seq.pp seq]
                        , %[$"hyp    : ", Seq.ppNoId hyp]
                        , %[$"concl  : ", Seq.pp concl]
                        , %[$"fresh  : ", PSet.pp fresh]
                        , %[$"result : ", &(map ppres res)]]])
         in
            res
         end

      fun apply ({ t = T { id, initial, hyp, concl, active, kept, fresh
                         , global, ruleAtoms, ... }
                 , kept = ksd, active = asd, conflicts, seq}, new) =
         let
            val _ = Log.trace (fn () => &[ %[$"-> Branch.apply: ", RuleId.pp id]])
            val out = RIndex.create { global = global, atoms = ruleAtoms, pp = Out.pp }
            val ks = match1 ( ksd, asd, conflicts, seq, hyp, fresh, concl
                            , global, ruleAtoms )
            val q_id = Seq.id seq
            (* Make sure the result sequents are ok. *)
            fun afn {rseq, renaming=_} =
               RSeq.invariant (rseq, { global = global, atoms = ruleAtoms
                                     , fresh = fresh})
            val _ = List.app afn ks
            fun addOut ((k, p), (r, ps)) =
               let in
                  case RSeq.combine (k, r) of
                     NONE => ()
                   | SOME q =>
                     let
                     (* val q = RSeq.restrict (q, ruleAtoms) *)
                     in
                        RIndex.insertRemoveSubsumed (out, q, p :: ps)
                     end
               end
            fun afn {rseq, renaming} =
               let
                  fun afn rps = addOut ((rseq, (q_id, renaming)), rps)
               in
                  (* out += k <+> active *)
                  RIndex.app afn active
                (* out += k <+> new *)
                ; RIndex.app afn new
               end
         in
            List.app afn ks
          (* out += new <+> kept *)
          ; RIndex.app
               (fn (r, ps) =>
                   RIndex.app (fn (k, p) => addOut ((k, p), (r, ps))) kept) new
          (* active += new *)
          ; RIndex.app
               (fn (q, ps) => if not (RIndex.subsumes (active, q)) then
                                 let in
                                    RIndex.removeSubsumed (active, q)
                                  ; RIndex.removeSubsumed (kept, q)
                                  ; RIndex.insert (active, q, ps)
                                 end
                              else ())
               new
          (* kept += ks *)
          ; if initial then () else
            List.app
               (fn { rseq, renaming } =>
                   if Seq.subsumes (seq, RSeq.seq rseq, {global=SOME global})
                      orelse RIndex.subsumes (active, rseq)
                   then ()
                   else RIndex.insertRemoveSubsumed (kept, rseq, (q_id, renaming)))
               ks
          ; Log.trace (fn () => &[ $"<- Branch.apply"
                                 , %[\\, %[ $"out: ", RIndex.ppSeqs out ]]])
          ; out
         end
   end

   (* ----------------------------------------------------------------------- *)

   structure Rule = struct
      datatype t = T of
         { id : RuleId.t
         , init : Branch.t
         , rest : Branch.t list
         , atoms : Atoms.t }

      fun concl (T { init, ...}) = Branch.concl init

      fun id (T {id, ...}) = id

      fun atoms (T {atoms, ...}) = atoms

      fun pp (T {init, rest, ...}) =
         &(List.separate \ (map Branch.pp (init :: rest)))

      fun size (T { init, rest, ... }) =
         List.foldl (fn (x, n) => Branch.size x + n)
            (Branch.size init) rest

      fun removeSubsumed (T { init, rest, ... }, seq) =
         List.app (fn b => Branch.removeSubsumed (b, seq)) (init :: rest)

      val match : (t * Kept.t * Active.t * Conflicts.t * Proof.t
                   * Seq.t) * Out.t -> Out.t =
         fn ((T {init, rest, ...}, ksd, asd, conflicts, _, seq), new) =>
            let
               fun fold (b, out) =
                  Branch.apply ({ t = b, kept = ksd, active = asd
                                , conflicts = conflicts, seq = seq }, out)
            in
               List.foldl fold new (init :: rest)
            end

      val create : Proof.t * Input.t * Seq.Ants.t -> t = fn
         (pdb, inp, global) =>
         let
            val (inp as Input.T { id, hyp, hyps, concl, fresh, ... }, _) =
               RuleInput.rename (inp, Subst.id)
            val _ = asserts (fn () =>
               PSet.all (fn a => not (Param.isFixed a)) fresh, "Rule.create")
            val ats = Atoms.make (VSet.empty, fresh)
            fun fix seq = Seq.fix' ats seq
            val (hyp, hyps, concl) = (fix hyp, map fix hyps, fix concl)
            (* Make sure to fix the fresh params in atoms. *)
            val ruleAtoms = Atoms.fix (Input.atoms inp, fresh)
            (* Now the params are fixed in the rule and atoms.  Fix them in fresh. *)
            val fresh = PSet.map Param.fix fresh
            val init =
               Branch.create
                  { hyp = hyp, hyps = hyps, concl = concl, fresh = fresh
                  , initial = true, global = global, ruleAtoms = ruleAtoms }
            fun aux (hyp, (acc, hyps)) =
               let
                  val hyps = case hyps of
                     [] => raise Impossible
                   | _ :: hyps => hyps
                  val b =
                     Branch.create
                        { hyp = hyp, hyps = hyps, concl = concl, fresh = fresh
                        , initial = false, global = global, ruleAtoms = ruleAtoms }
               in
                  (b :: acc, hyps)
               end
            val (rest, _) = List.foldl aux ([], hyps) hyps
         in
            Proof.addRule (pdb, inp)
          ; T { id = id
              , init = init
              , rest = rev rest
              , atoms = ruleAtoms }
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
      , proof : Proof.t
      , global : Seq.Ants.t }

   type parseable = t
   type printable = t

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

   (* Remove rules subsumed by the active database. *)
   fun removeSubsumed (T { rules as ref l, conclIndex, hypIndex, global, ...}, seq) =
      let
         val (obs, ok) =
            List.partition (fn r => Seq.subsumes (seq, Rule.concl r, {global=SOME global})) l
         fun f (Rule.T { init, ... }) =
            let in
               Index.remove (conclIndex, Seq.id (Branch.concl init))
             ; List.app
                  (fn h => Index.remove (hypIndex, Seq.id h))
                  (Branch.hyps init)
            end
      in
         List.app f obs
       ; rules := ok
       ; List.app (fn b => Rule.removeSubsumed (b, seq)) ok
      end

   fun create { proof, goal, initialSeqs, preds, rules, conflicts, kept
              , active, global } =
      let
         val rules =
            List.map (fn r => Rule.create (proof, r, global)) rules
         val hypIndex = Index.create {global=SOME global, pp = PP.unit}
         val conclIndex = Index.create {global=SOME global, pp = PP.unit}
         fun f (Rule.T { init, ... }) =
            let
               val seq = Branch.concl init
            in
               Index.insert (conclIndex, Seq.id seq, seq, ())
             ; List.app
                  (fn hyp => Index.insert (hypIndex, Seq.id hyp, hyp, ()))
                  (Branch.hyps init)
            end
         val _ = List.app f rules
         val conflicts = Conflicts.restrict (conflicts, preds)
         val _ = Log.info (fn () => Conflicts.pp conflicts)
         val t = T { rules = ref rules
                   , conclIndex = conclIndex
                   , hypIndex = hypIndex
                   , goal = goal
                   , initialSeqs = initialSeqs
                   , preds = preds
                   , conflicts = conflicts
                   , kept = kept
                   , active = active
                   , proof = proof
                   , global = global }
      in
         t
      end

   datatype out = Out of
      { seq : Seq.t
      , prio : int
      , parents : Proof.parents }

   fun match ( t as T { rules = ref rs, conflicts, kept, active, proof
                      , global, ... }, seq) =
      let
         val _ = Log.trace (fn () => $"-> Rule.match")
         fun ffn (r, rest) =
            let
               val id = Rule.id r
               val atoms = Rule.atoms r
               val new =
                  RIndex.create {global = global, atoms = atoms, pp = Out.pp}
               val out =
                  Rule.match ((r, kept, active, conflicts, proof, seq), new)
               fun mfn (r, ps) =
                  let
                     val atoms = Atoms.unfix (Atoms.union (RSeq.atoms r, atoms))
                     val theta = Subst.renameAtoms (Atoms.unfix atoms)
                     val (seq, rth) = RSeq.dest (RSeq.unfix r)
                     val seq = Seq.apply (seq, theta)
                     val unifier = Subst.compose (rth, theta)
                     val parents =
                        Proof.Parents
                           { rid = id
                           , parents =
                             rev (map (fn (id, t) => {id=id, renaming=t}) ps)
                           , unifier = unifier }
                  in
                     Out { seq = seq
                         , prio = prio (t, seq)
                         , parents = parents }
                  end
               val out = List.map mfn (RIndex.toListi out)
            in
               out @ rest
            end
         val out = List.foldl ffn [] rs
      in
         Log.trace (fn () => $"<- Rule.match");
         (* Log.trace (fn () => &[ %[$"Rule.match: ", PP.paren (PP.int (length out))] *)
         (*                      , %[\\, &(map (fn Out { seq, ... } => %[Seq.ppNoId seq]) out)]]); *)
         out
      end

   val () = noWarnUnused (fn _ : printable * parseable => ())
end
