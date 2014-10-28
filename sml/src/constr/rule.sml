
structure Rule :> Rule = struct
   structure C = CFormula
   structure Id = RuleId
   structure PSet = Param.Set
   structure VSet = Var.Set
   structure Ants = Seq.Ants
   structure Pre = Seq.Pre

   open General
   open PP.Ops

   datatype t = T of
      { id : RuleId.t
      , hyp : Pre.t
      , hyps : Pre.t list
      , concl : Pre.t
      , constr : CFormula.t
      , fresh : Param.set
      , atoms : Atoms.t }
   type printable = t

   fun create {hyps, concl, fresh, constr} = case hyps of
      [] => raise Impossible
    | hyp :: hyps =>
      let
         val hyp = Pre.unfix hyp
         val hyps = map Pre.unfix hyps
         val fresh = PSet.map Param.unfix fresh
         val () = assert ( fn () => PSet.all (not o Param.isFixed) fresh
                         , fn () => $"fixed params")
         (* Don't grab the atoms from the constr.  They will be
            a subset of the atoms in the hyps and concl when we
            quantify over the difference when making a sequent. *)
         val atoms =
            foldl
               (fn (q, ats) => Atoms.union (ats, Pre.atoms q))
               Atoms.empty (concl :: hyp :: hyps)
         val afn = fn
            Left x => not (Var.isFixed x)
          | Right x => not (Param.isFixed x)
         val () = assert ( fn () => Atoms.all afn atoms
                         , fn () => $"fixed atoms")
         (* Restrict fresh to params occurring in the sequent. *)
         val (_, params) = Atoms.dest atoms
         val fresh = PSet.intersection (fresh, params)
         fun fix seq = Pre.fix' (Atoms.make (VSet.empty, fresh)) seq
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
           , atoms = atoms
           , constr = constr }
      end

   fun id (T {id, ...}) = id
   fun firstHyp (T {hyp, ...}) = hyp
   fun hyps (T {hyp, hyps, ...}) = hyp :: hyps
   fun concl (T {concl, ...}) = concl
   fun fresh (T {fresh, ...}) = fresh
   fun atoms (T {atoms, ...}) = atoms
   fun constr (T {constr, ...}) = constr

   fun ok r =
      let
         val concl = concl r
      in
         not (List.exists (fn h => Pre.eq (h, concl)) (hyps r))
      end

   fun ofFocus (Focus.Rule.T {hyps, concl, fresh, constr, ...}) =
      let
         val hyps = map Pre.ofFocus hyps
         val concl = Pre.ofFocus concl
         val fresh = PSet.map Param.fix fresh
      in
         create {constr = constr, hyps = hyps, concl = concl, fresh = fresh}
      end

   fun pp (T {id, hyp, hyps, concl, fresh, constr, ...}) =
      &[ &(map Pre.pp (hyp :: hyps))
       , %[$"--------------------------------------",
           PSet.pp fresh, \, PP.paren(%[$"R", Id.pp id])]
       , %[CFormula.pp constr, \, $"|", \, Pre.pp concl]
       ]

   fun preds (T {hyp, hyps, concl, ...}) =
      foldl
         (fn (q, acc) => Pred.Set.union (acc, Pre.preds q))
         Pred.Set.empty (concl :: hyp :: hyps)

   fun prio (T {concl = (ants, cons), constr, hyps, ...}, {goal}) =
      Seq.prio {seq = Seq.new (constr, ants, cons), goal = goal}
         - 5 * length hyps

   fun rename r =
      let
         val theta = Subst.renameAtoms (atoms r)
         fun mfn q = Pre.apply (Pre.unfix q, theta)
         val hyps = map mfn (hyps r)
         val concl = mfn (concl r)
         val fresh = PSet.map (fn a => Subst.applyP (Param.unfix a, theta)) (fresh r)
         val constr = CFormula.apply (constr r, theta)
         val r' = create {constr = constr, hyps = hyps, concl = concl, fresh = fresh}
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

   fun match { seq, rule as T {hyp, hyps, concl, fresh, constr, atoms, ...}
             , global, simp} =
      let
         val constr = C.fill {fillee = constr, filler = Seq.constr seq}
      in
         Log.trace (fn () =>
            &[ $"Rule.match:"
             , %[\\, &[ &[$"Seq:", %[\\, Seq.pp seq]]
                      , &[$"Rule:", %[\\, pp rule]]
                      ]]])
       ; case Seq.match { seq = seq
                        , hyp = hyp
                        , concl = concl
                        , fresh = fresh
                        , global = global
                        , atoms = atoms }
         of [] => M.NoMatch
          | newConcls =>
            case hyps of
               [] =>
               let
                  fun mfn {concl = (ants, cons), csubst, filledHole = _} =
                     let
                        val _ = printl "10"
                        val theta = CSubst.subst csubst
                        val constr = CSubst.constr (csubst, constr)
                        val constr = C.unfix constr
                        val cats = C.atoms constr
                        val _ = printl "11"
                        val lats = Pre.atoms concl
                        val xs = Atoms.vars (Atoms.difference (cats, lats))
                        (* Existentially quantify free variables. *)
                        val constr = C.quantifyVars (constr, xs)
                        (* Universally quantify fixed parameters. *)
                        val _ = printl "12"
                        val _ = PP.ppl (C.pp constr)
                        val constr = C.quantifyParams (constr, PSet.map Param.unfix fresh)
                        (* Apply the theory-specific simplifier. *)
                        val _ = printl "20"
                        val (thetaS, constr) = simp constr
                        val (ants, cons) = Pre.apply ((ants, cons), thetaS)
                        val concl = Seq.new (constr, ants, cons)
                        val (concl, renaming) = Seq.rename (concl, Subst.id)
                        val theta = Subst.composes [Subst.unfix theta, thetaS, renaming]
                     in
                        (concl, theta)
                     end
               in
                  M.NewSeqs (map mfn newConcls)
               end
             | _ =>
               let
                  fun mfn { concl, csubst, filledHole } =
                     let
                        val theta = CSubst.subst csubst
                        fun mfn q =
                           let
                              val q = Pre.apply (q, theta)
                           in
                              if filledHole then Pre.fill (q, snd concl)
                              else q
                           end
                        val hyps = map mfn hyps
                        val constr = CSubst.constr (csubst, constr)
                        val rule =
                           create {constr = constr, hyps=hyps, concl=concl, fresh=fresh}
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
   fun contract (T {hyp, hyps, concl, fresh, constr, ...}) =
      let
         fun mfn (concl, csubst) =
            let
               val theta = CSubst.subst csubst
               val hyps = map (fn h => Pre.apply (h, theta)) (hyp :: hyps)
               val fresh = PSet.map (fn p => Subst.applyP (p, theta)) fresh
               val constr = CSubst.constr (csubst, C.apply (constr, theta))
            in
               (create {constr = constr, hyps = hyps, concl = concl, fresh = fresh}, theta)
            end
      in
         map mfn (Pre.contract (concl, {global=Ants.empty}))
      end

   val () = noWarnUnused (fn _ : printable => ())
end


