
structure Rule :> Rule = struct
   structure C = Focus
   structure Id = RuleId
   structure PSet = Param.Set
   structure VSet = Var.Set
   structure Ants = Seq.Ants

   open General
   open PP.Ops

   datatype t = T of
      { id : RuleId.t
      , hyp : Seq.t
      , hyps : Seq.t list
      , concl : Seq.t
      , fresh : Param.set
      , atoms : Atoms.t }
   type printable = t

   fun create {hyps, concl, fresh} = case hyps of
      [] => raise Impossible
    | hyp :: hyps =>
      let
         val hyp = Seq.unfix hyp
         val hyps = map Seq.unfix hyps
         val fresh = PSet.map Param.unfix fresh
         val () = assert' (fn () => PSet.all (not o Param.isFixed) fresh)
         val atoms =
            foldl
               (fn (q, ats) => Atoms.union (ats, Seq.atoms q))
               Atoms.empty (concl :: hyp :: hyps)
         val afn = fn
            Left x => not (Var.isFixed x)
          | Right x => not (Param.isFixed x)
         val () = assert' (fn () => Atoms.all afn atoms)
         (* Restrict fresh to params occurring in the sequent. *)
         val (_, params) = Atoms.dest atoms
         val fresh = PSet.intersection (fresh, params)
         fun fix seq = Seq.fix' (Atoms.make (VSet.empty, fresh)) seq
         val hyp = fix hyp
         val hyps = map fix hyps
         val concl = fix concl
         val fresh = Param.Set.map Param.fix fresh
      in
         T { id = Id.next ()
           , hyp = hyp
           , hyps = hyps
           , concl = concl
           , fresh = fresh
           , atoms = atoms }
      end

   fun id (T {id, ...}) = id
   fun firstHyp (T {hyp, ...}) = hyp
   fun hyps (T {hyp, hyps, ...}) = hyp :: hyps
   fun concl (T {concl, ...}) = concl
   fun fresh (T {fresh, ...}) = fresh
   fun atoms (T {atoms, ...}) = atoms

   fun ok r =
      let
         val concl = concl r
      in
         not (List.exists (fn h => Seq.eq (h, concl)) (hyps r))
      end

   fun ofFocus (C.Rule.T {hyps, concl, fresh, ...}) =
      let
         val hyps = map Seq.ofFocus hyps
         val concl = Seq.ofFocus concl
         val fresh = PSet.map Param.fix fresh
      in
         create {hyps = hyps, concl = concl, fresh = fresh}
      end

   fun pp (T {id, hyp, hyps, concl, fresh, ...}) =
      &[ &(map Seq.ppNoId (hyp :: hyps))
       , %[$"--------------------------------------",
           PSet.pp fresh, \, PP.paren(%[$"R", Id.pp id])]
       , Seq.ppNoId concl
       ]

   fun preds (T {hyp, hyps, concl, ...}) =
      foldl
         (fn (q, acc) => Pred.Set.union (acc, Seq.preds q))
         Pred.Set.empty (concl :: hyp :: hyps)

   fun prio (r, {goal}) =
      Seq.prio {seq = concl r, goal = goal} - 5 * length (hyps r)

   fun rename r =
      let
         val theta = Subst.renameAtoms (atoms r)
         fun mfn q = Seq.apply (Seq.unfix q, theta)
         val hyps = map mfn (hyps r)
         val concl = mfn (concl r)
         val fresh = PSet.map (fn a => Subst.applyP (Param.unfix a, theta)) (fresh r)
         val r' = create {hyps=hyps, concl=concl, fresh=fresh}
      in
         (r', theta)
      end

   structure Match = struct
      type rule = t
      datatype t = NewRules of (rule * Subst.t) list
                 | NewSeqs of (Seq.t * Subst.t) list
                 | NoMatch
      val pp = fn
         NewSeqs qs => &(map (fn (q, s) => &[Seq.pp q, Subst.pp s]) qs)
       | NewRules rs => &(map (fn (r, s) => &[pp r, Subst.pp s]) rs)
       | NoMatch => $"NoMatch"
   end
   structure M = Match

   fun match {seq, rule as T {hyp, hyps, concl, fresh, ...}, global} =
      let
         val ats = atoms rule
      in
         case Seq.match { seq = seq
                        , hyp = hyp
                        , concl = concl
                        , fresh = fresh
                        , global = global
                        , atoms = ats }
         of [] => M.NoMatch
          | newConcls =>
            case hyps of
               [] =>
               let
                  fun mfn { concl, theta, filledHole=_ } =
                     let
                        (* val concl = Seq.unfix concl *)
                        val (concl, renaming) = Seq.rename (concl, Subst.id)
                        val theta = Subst.compose (Subst.unfix theta, renaming)
                     in
                        (concl, theta)
                     end
               in
                  M.NewSeqs (map mfn newConcls)
               end
             | _ =>
               let
                  fun mfn { concl, theta, filledHole } =
                     let
                        fun mfn q =
                           let
                              val q = Seq.apply (q, theta)
                           in
                              if filledHole then Seq.fill (q, Seq.cons concl)
                              else q
                           end
                        val hyps = map mfn hyps
                        val rule = create {hyps=hyps, concl=concl, fresh=fresh}
                        val (rule, renaming) = rename rule
                        val theta = Subst.compose (Subst.unfix theta, renaming)
                     in
                        (rule, theta)
                     end
               in
                  M.NewRules (map mfn newConcls)
               end
      end

   (* Contract the conclusion of the rule with the globals. *)
   fun contract (T {hyp, hyps, concl, fresh, ...}) =
      let
         fun mfn (concl, theta) =
            let
               val hyps = map (fn h => Seq.apply (h, theta)) (hyp :: hyps)
               val fresh = PSet.map (fn p => Subst.applyP (p, theta)) fresh
            in
               (create {hyps = hyps, concl = concl, fresh = fresh}, theta)
            end
      in
         map mfn (Seq.contract (concl, {global=Ants.empty}))
      end

   val () = noWarnUnused (fn _ : printable => ())
end


