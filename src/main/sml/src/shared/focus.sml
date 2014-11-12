
structure Focus :> Focus = struct

   structure P = Fragment
   structure F = LFormula
   structure F' = PFormula
   structure C = CFormula
   structure U = Unicode
   structure PSet = Param.Set

   open General
   open PP.Ops

   (* An antecedent (on the left of the sequent arrow) is either
      negative or is a positive atom.

      Invariant: In 'PosAtom p', p must be for the form (F.PAtom a)
      Don't include only the atom because we need the label for the
      proposition. *)
   structure Left = struct
      structure Ops = struct
         datatype t =
            Neg of F.neg
          | PosAtom of F.pos
      end
      datatype t = datatype Ops.t
      val pp = fn
         Neg f => F.neg F.pp f
       | PosAtom f => F.pos F.pp f

      val freeze = fn
         Neg f => Neg (F.neg F.freeze f)
       | PosAtom p => PosAtom (F.pos F.freeze p)

      val label = fn
         PosAtom p => F.pos F.label p
       | Neg p => F.neg F.label p

      val pred = Rel.pred o label

      val atoms = fn
         Neg f => F.neg F.atoms f
       | PosAtom f => F.pos F.atoms f

      fun globalize global ls =
         List.filter
            (fn l => not (List.genMem Rel.eq (label l, global)))
            ls

      val pformula = fn
         Neg n => F'.Down (F.neg F.pformula n)
       | PosAtom m => F.pos F.pformula m
   end
   open Left.Ops

   (* A consequent (on the right of the sequent arrow) is either

      1) positive
      2) a negative atom
      3) a variable (called Ξ) that gets instantiated by matching.

      Invariant: In 'NegAtom n', n must be for the form (F.NAtom m) *)
   structure Right = struct
      structure Ops = struct
         datatype t =
            Pos of F.pos
          | NegAtom of F.neg
          | Var
      end
      datatype t = datatype Ops.t

      val freeze = fn
         Pos f => Pos (F.pos F.freeze f)
       | NegAtom p => NegAtom (F.neg F.freeze p)
       | Var => Var

      val label = fn
         Pos p => SOME (F.pos F.label p)
       | NegAtom p => SOME (F.neg F.label p)
       | Var => NONE

      val atoms = fn
         Pos p => F.pos F.atoms p
       | NegAtom p => F.neg F.atoms p
       | Var => Atoms.empty

      val pformula = fn
         Pos n => F'.Up (F.pos F.pformula n)
       | NegAtom m => F.neg F.pformula m
       | Var => F'.Up F'.Zero
   end
   open Right.Ops

   structure Side = struct
      type t = (Left.t, Right.t) either
      val label = fn
         Left l => SOME (Left.label l)
       | Right r => Right.label r
   end

   structure Ants = struct
      type t = Left.t list
      val atoms : t -> Atoms.t =
         foldl (fn (l, ats) => Atoms.union (Left.atoms l, ats))
            Atoms.empty
   end

   structure Seq = struct
      type t = Left.t list * Right.t
      type printable = t
      val pp : t -> PP.t =
         let
            fun left (Neg n) = F.neg F.pp n
              | left (PosAtom p) = F.pos F.pp p
            fun right (Pos p) = F.pos F.pp p
              | right (NegAtom n) = F.neg F.pp n
              | right (Var) = $U.cdot
            val pp_ants = fn
               [] => $U.cdot
             | l => PP.fsep (PP.punctuate PP.comma (map left l))
            fun pp (ants, cons) =
               PP.fsep [pp_ants ants, $U.vdash, right cons]
         in
            pp
         end
      fun freeze (ls, r) = (map Left.freeze ls, Right.freeze r)
      fun atoms (ls, r) = Atoms.unions (Right.atoms r :: map Left.atoms ls)
      fun globalize global (ls, r) = (Left.globalize global ls, r)
      fun pformula (ls, r) = F'.mkLolli (map Left.pformula ls, Right.pformula r)

      val () = noWarnUnused (fn _ : printable => ())
   end

   structure CSeq = struct
      datatype t = T of
         { seq : Seq.t
         , constr : CFormula.t
         , frozen : Func.set }
      type printable = t
      fun pp (T {seq, constr, frozen}) =
         %%[C.pp constr, $"|", Seq.pp seq, Func.Set.pp frozen]
      fun freeze (T {seq, constr, frozen}) =
         T {seq = Seq.freeze seq, constr = C.freeze constr, frozen = frozen}
   end

   structure Rule = struct
      datatype t = T of
         { hyps : Seq.t list
         , concl : Seq.t
         , fresh : Param.set
         , constr : CFormula.t
         , proof : P.t }
      type printable = t

      type t' = Seq.t list * Seq.t * Param.set * CFormula.t * P.t

      fun constr (T { constr, ... }) = constr

      fun finish (hyps, concl, fresh, constr, proof) =
         T { hyps = hyps
           , concl = concl
           , fresh = fresh
           , constr=C.simplify constr
           , proof=proof }

      fun globalize global (hyps, concl, fresh, constr, proof) =
         ( map (Seq.globalize global) hyps
         , Seq.globalize global concl
         , fresh, constr, proof )

      fun pp (T {hyps, concl, fresh, constr, ...}) =
         &[ &(map Seq.pp hyps)
          , %[$"--------------------------------", Param.Set.pp fresh]
          , %[C.pp constr, \, $"|", \, Seq.pp concl]]

      val () = noWarnUnused (fn _ : printable => (pp))
   end

   structure Stable = struct
      datatype t = T of
         { formula: PFormula.neg
         , lformula: LFormula.neg
         , seqs: CSeq.t list
         , proof: P.t
         , bipolarPreds : Pred.set
         , conflicts: Conflicts.t }
      type printable = t
      fun seqs (T {seqs, ...}) = seqs
      fun pp (T {seqs, ...}) =
         &[ &[$"seqs", %[\\, &(map CSeq.pp seqs)]]
          ]
   end

   structure Foci = struct
      datatype t = T of
         { rules: Rule.t list
         , goal: Seq.t
         , global: Left.t list
         , constr: CFormula.t }
      type printable = t
      fun pp (T {rules, goal, global, constr} : printable) =
         let

         in
            &[ &[$"Rules  : ", %[\\, &(List.separate \ (map Rule.pp rules))]]
             , &[$"Goal   : ", Seq.pp goal]
             , &[$"Global : ", %[\\, &(map Left.pp global)]]
             , &[$"Constr : ", %[\\, C.pp constr]]]
         end
   end

   (* Invert a sequent until all left propositions are positive in all
      branches. *)
   val rec stabilizeL :
      Left.t list * F.pos list * Right.t -> Seq.t list * Param.set * C.t * P.t = fn
      (gamma, [], r) => ([(gamma, r)], PSet.empty, C.Hole, P.Node P.Leaf)
    | (gamma, f :: delta, r) =>
      let
         val lf = F.pos F.label f
      in
         case F.pos F.expose f of
            F.PAtom rel =>
            if Rel.isConstr rel then
               let
                  val (seqs, fresh, psi, recon) =
                     stabilizeL (gamma, delta, r)
                  val psi = C.Imp (rel, psi)
               in
                  (seqs, fresh, psi, recon)
               end
            else
               stabilizeL (PosAtom f :: gamma, delta, r)
          | F.Tensor (a,b) =>
            let
               val la = F.pos F.label a
               val lb = F.pos F.label b
               val (seqs, fresh, psi, recon) =
                  stabilizeL (gamma, a :: b :: delta, r)
            in
               (seqs, fresh, psi, P.mapr (fn d => SC.TensorL (lf, (la, lb), d)) recon)
            end
          | F.One =>
            let
               val (seqs, fresh, psi, recon) = stabilizeL (gamma, delta, r)
            in
               (seqs, fresh, psi, P.mapr (fn d => SC.OneL (lf, d)) recon)
            end
          | F.Sum (a,b) =>
            let
               val la = F.pos F.label a
               val lb = F.pos F.label b
               val (seqs1, fresh1, psi1, recon1) = stabilizeL (gamma, a :: delta, r)
               val (seqs2, fresh2, psi2, recon2) = stabilizeL (gamma, b :: delta, r)
               val fresh = PSet.union(fresh1, fresh2)
               val psi = C.And (psi1, psi2)
            in
               ( seqs1 @ seqs2, fresh, psi
               , P.mapr2 (fn (d,d') => SC.PlusL (lf, (la,d), (lb,d')))
                    (recon1,recon2))
            end
          | F.Zero => ([], PSet.empty, C.Top, P.Leaf (SC.ZeroL lf))
          | F.Down a =>
            let
               val la = F.neg F.label a
               val (seqs, fresh, psi, recon) = stabilizeL (Neg a :: gamma, delta, r)
            in
               (seqs, fresh, psi, P.mapr (fn d => SC.DownL (lf, (la, d))) recon)
            end
          | F.Ex((x, _), a) =>
            let
               val p = Param.next ()
               val a = F.pos (F.apply1 (x, Term.Param p)) a
               val la = F.pos F.label a
               val (seqs, fresh, psi, recon) = stabilizeL (gamma, a :: delta, r)
               val fresh = Param.Set.add(fresh, p)
            in
               (seqs, fresh, psi, P.mapr (fn d => SC.ExL(lf, (la, p), d)) recon)
            end
      end

   (* Invariant: if stablizeR (gamma, delta, r) --> (hyps, recon)
                then given any derivations {D} of hyps, applying recon to D
                will yield a derivation of gamma U delta ==> r
                (with antecedents extended by the antecedents of D)

      Invert a sequent until the rhs is negative in all branches.
   *)
   val rec stabilizeR : Left.t list * F.pos list * F.neg
      -> Seq.t list * Param.set * C.t * P.t = fn
      (gamma, delta, f) =>
      case F.neg F.expose f of
         F.NAtom _ => stabilizeL (gamma, delta, NegAtom f)
       | F.With (a, b) =>
         let
            val (hyps1, fresh1, psi1, recon1) = stabilizeR (gamma, delta, a)
            val (hyps2, fresh2, psi2, recon2) = stabilizeR (gamma, delta, b)
            val fresh = PSet.union(fresh1, fresh2)
            val psi = C.And (psi1, psi2)
         in
            (hyps1 @ hyps2, fresh, psi, P.mapr2 SC.WithR (recon1, recon2))
         end
       | F.Top => ([], PSet.empty, C.Top, P.Leaf SC.TopR)
       | F.Lolli (a, b) =>
         let
            val la = F.pos F.label a
            val (hyps, fresh, psi, recon) = stabilizeR (gamma, a :: delta, b)
         in
            (hyps, fresh, psi, P.mapr (fn d => SC.LolliR (la, d)) recon)
         end
       | F.Up a =>
         let
            val (qs, fresh, psi, recon) = stabilizeL (gamma, delta, Pos a)
         in
            (qs, fresh, psi, P.mapr SC.UpR recon)
         end
       | F.BiLolli (a,b) =>
         if !Parameters.Focus.useBiimpContinue then
            let
               (* The case analysis searching for arrows makes a big difference.
                  e.g. a factor of 10 on SYJ006 *)
               val a' = a
               val b' = b
               (*  Freshening didn't work at all *)
               (* val a' = F.freshen a *)
               (* val b' = F.freshen b *)
               fun lab f = case F.neg F.expose f of
                  F.Up f => F.pos F.label f
                | _ => F.neg F.label f
               val (gamma1, delta1) = case F.neg F.expose a of
                  F.Up a => (gamma, a :: delta)
                | _ => (Neg a :: gamma, delta)
               val (hyps1, fresh1, psi1, recon1) = stabilizeR (gamma1, delta1, b)
               val (gamma2, delta2) = case F.neg F.expose b' of
                  F.Up b' => (gamma, b' :: delta)
                | _ => (Neg b' :: gamma, delta)
               val (hyps2, fresh2, psi2, recon2) = stabilizeR (gamma2, delta2, a')
               val fresh = PSet.union(fresh1, fresh2)
               val psi = C.And (psi1, psi2)
               fun rleft r = case F.neg F.expose a of
                  F.Up _ => SC.UpR r
                | _ => r
               fun rright r = case F.neg F.expose b' of
                  F.Up _ => SC.UpR r
                | _ => r
               fun rfun (d1, d2) =
                  SC.BiLolliR ((lab a, rleft d1), (lab b', rright d2))
            in
               (hyps1 @ hyps2, fresh, psi, P.mapr2 rfun (recon1, recon2))
            end
         else
            let
               val la = F.neg F.label a
               val lb = F.neg F.label b
               val (hyps1, fresh1, psi1, recon1) =
                  stabilizeR (Neg a :: gamma, delta, b)
               val (hyps2, fresh2, psi2, recon2) =
                  stabilizeR (Neg b :: gamma, delta, a)
               val fresh = PSet.union(fresh1, fresh2)
               val psi = C.And (psi1, psi2)
               fun rfun (d1, d2) = SC.BiLolliR ((la, d1), (lb, d2))
            in
               (hyps1 @ hyps2, fresh, psi, P.mapr2 rfun (recon1, recon2))
            end
       | F.All ((x, _), a) =>
         let
            val p = Param.next ()
            val a = F.neg (F.apply1 (x, Term.Param p)) a
            val (qs, fresh, psi, recon) = stabilizeR (gamma, delta, a)
            val fresh = PSet.add(fresh, p)
         (* val psi = C.All ((x, t), psi) *)
         (* val psi = C.All ((x, t), C.paramSubst (psi, (p, x))) *)
         in
            (qs, fresh, psi, P.mapr (fn d => SC.AllR(p, d)) recon)
         end

   (* focusR atoms r
      atoms is a list of atoms that occur both positively and negatively.
    *)
   val rec focusR : Pred.set * Right.t -> Rule.t' list = fn
      (atoms, f) =>
      case f of
         Var => []
       | NegAtom _ => []
       | Pos f' =>
         case F.pos F.expose f' of
            F.PAtom rel =>
            if Rel.isConstr rel then
               [ ([], ([], f), PSet.empty, C.Atom rel, P.Leaf (SC.Init rel)) ]
            else if Pred.Set.mem (atoms, Rel.pred rel)
            then [([], ([PosAtom f'], f), PSet.empty, C.Top, P.Leaf (SC.Init rel))]
            else []
          | F.Tensor (a, b) =>
            let
               val rs1 = focusR (atoms, Pos a)
               val rs2 = focusR (atoms, Pos b)
               fun merge ((hyps1, (ants1, _), fresh1, psi1, recon1),
                          (hyps2, (ants2, _), fresh2, psi2, recon2)) =
                  ( hyps1 @ hyps2
                  , (ants1 @ ants2, f)
                  , PSet.union (fresh1, fresh2)
                  , C.And (psi1, psi2)
                  , P.mapr2 SC.TensorR (recon1,recon2))
            in
               map merge (List.allPairs Fun.id (rs1, rs2))
            end
          | F.One => [([], ([], f), PSet.empty, C.Top, P.Leaf SC.OneR)]
          | F.Sum (a,b) =>
            let
               val rs1 = focusR (atoms, Pos a)
               val rs2 = focusR (atoms, Pos b)
               fun mapfn1 (hyps, (ants, _), fresh1, psi1, recon)  =
                  (hyps, (ants, f), fresh1, psi1, P.mapr SC.PlusR1 recon)
               fun mapfn2 (hyps, (ants, _), fresh2, psi2, recon)  =
                  (hyps, (ants, f), fresh2, psi2, P.mapr SC.PlusR2 recon)
            in
               map mapfn1 rs1 @ map mapfn2 rs2
            end
          | F.Zero => []
          | F.Down a =>
            let
               val (hyps, fresh, psi, recon) = stabilizeR ([], [], a)
            in
               [(hyps, ([], f), fresh, psi, P.mapr SC.DownR recon)]
            end
          | F.Ex((x, _), a) =>
            let
               val X = Var.next ()
               val a = F.pos (F.apply1 (x, Term.Var X)) a
               val rs = focusR(atoms, Pos a)
               fun mapFn (hyps, (ants, _), fresh, psi, recon)  =
                  (hyps, (ants, f), fresh, psi,
                   P.mapr (fn r => SC.ExR(Term.Var X, r)) recon)
            in
               map mapFn rs
            end

   (* Remove a label from the antecedents *)
   val filterAnts : Rel.t * Left.t list -> Left.t list = fn
      (l, ants) => List.filter (fn a => not (Rel.eq (l, Left.label a))) ants

   val rec focusL : Pred.set * Left.t -> Rule.t' list = fn
      (atoms, f) =>
      case f of
         PosAtom _ => []
       | Neg f' =>
         let
            val lf = F.neg F.label f'
         in
            case F.neg F.expose f' of
               F.NAtom rel =>
               if Pred.Set.mem (atoms, Rel.pred rel) then
                  [([], ([f], NegAtom f'), PSet.empty, C.Top, P.Leaf (SC.Init rel))]
               else []
             | F.With (a,b) =>
               let
                  val la = F.neg F.label a
                  val lb = F.neg F.label b
                  val rs1 = focusL (atoms, Neg a)
                  val rs2 = focusL (atoms, Neg b)
                  fun mapfn1 (hyps, (ants, cons), fresh, psi, recon) =
                     let
                        val recon' = P.mapr (fn d => SC.WithL1 (lf, (la, d))) recon
                        val ants' = f :: filterAnts (la, ants)
                     in
                        (hyps, (ants', cons), fresh, psi, recon')
                     end
                  fun mapfn2 (hyps, (ants, cons), fresh, psi, recon) =
                     let
                        val recon' = P.mapr (fn d => SC.WithL2 (lf, (lb, d))) recon
                        val ants' = f :: filterAnts (lb, ants)
                     in
                        (hyps, (ants', cons), fresh, psi, recon')
                     end
               in
                  map mapfn1 rs1 @ map mapfn2 rs2
               end
             | F.Top => []
             | F.Lolli (a,b) =>
               let
                  val lb = F.neg F.label b
                  val rs1 = focusL (atoms, Neg b)
                  val rs2 = focusR (atoms, Pos a)
                  fun merge ((hyps1, (ants1, cons1), fresh1, psi1, recon1),
                             (hyps2, (ants2, _), fresh2, psi2, recon2)) =
                     let
                        val fresh = PSet.union (fresh1, fresh2)
                        val psi = C.And (psi1, psi2)
                        val hyps = hyps1 @ hyps2
                        val ants = f :: filterAnts (lb, ants1 @ ants2)
                        val recon =
                           P.mapr2 (fn (d,d') => SC.LolliL (lf, (lb, d), d'))
                              (recon1, recon2)
                     in
                        (hyps, (ants, cons1), fresh, psi, recon)
                     end
               in
                  map merge (List.allPairs Fun.id (rs1,rs2))
               end
             | F.BiLolli (a, b) =>
               let
                  val lab = F.neg F.label
                  val a' = a
                  val b' = b
                  val rs1 = focusL (atoms, Neg b)
                  val (a_hyps, a_fresh, a_psi, a_recon) = stabilizeR ([], [], a)
                  val rs2 = focusL (atoms, Neg a')
                  val (b_hyps, b_fresh, b_psi, b_recon) = stabilizeR ([], [], b')
                  fun mapfn1 (hyps, (ants, cons), fresh, psi, recon) =
                     let
                        val hyps' = hyps @ a_hyps
                        val ants' = f :: filterAnts (lab b, ants)
                        val fresh' = Param.Set.union (fresh, a_fresh)
                        val psi' = C.And (psi, a_psi)
                        val recon' = P.mapr2 (fn (d, d') => SC.BiLolliL1 ((lab b, d), d', lf))
                                        (recon, a_recon)
                     in
                        (hyps', (ants', cons), fresh', psi', recon')
                     end
                  fun mapfn2 (hyps, (ants, cons), fresh, psi, recon)  =
                     let
                        val hyps' = hyps @ b_hyps
                        val ants' = f :: filterAnts (lab a', ants)
                        val fresh' = Param.Set.union (fresh, b_fresh)
                        val psi' = C.And (psi, b_psi)
                        val recon' = P.mapr2 (fn (d, d') => SC.BiLolliL2 ((lab a', d), d', lf))
                                        (recon, b_recon)
                     in
                        (hyps', (ants', cons), fresh', psi', recon')
                     end
               in
                  map mapfn1 rs1 @ map mapfn2 rs2
               end
             | F.Up a =>
               let
                  val la = F.pos F.label a
                  val (hyps, fresh, psi, recon) = stabilizeL ([], [a], Var)
                  val recon' = P.mapr (fn d => SC.UpL (lf, (la, d))) recon
               in
                  [(hyps, ([f], Var), fresh, psi, recon')]
               end
             | F.All ((x, _), a) =>
               let
                  val X = Var.next ()
                  val a = F.neg (F.apply1 (x, Term.Var X)) a
                  val la = F.neg F.label a
                  val rs = focusL (atoms, Neg a)
                  fun mapFn (hyps, (ants, cons), fresh, psi, recon)  =
                     let
                        val recon' = P.mapr (fn d => SC.AllL(lf, (la, Term.Var X), d)) recon
                        val ants' = f :: filterAnts(la, ants)
                     in
                        (hyps, (ants', cons), fresh, psi, recon')
                     end
               in
                  map mapFn rs
               end
         end

   val rec stabL
      :  C.t * Param.set * Left.t list * F.pos list * Right.t
      -> CSeq.t list * P.t = fn
      (psi, fresh, gamma, [], r) =>
      let
         val frozen = PSet.fold (fn (a, s) => Func.Set.add (s, Func.ofParam a))
                         Func.Set.empty fresh
      in
         ([CSeq.T {seq = (gamma, r), constr = psi, frozen = frozen}], P.Node P.Leaf)
      end
    | (psi, fresh, gamma, f :: delta, r) =>
      let
         val lf = F.pos F.label f
      in
         case F.pos F.expose f of
            F.PAtom rel =>
            if Rel.isConstr rel then
               stabL (C.And (C.Atom rel, psi), fresh, PosAtom f :: gamma, delta, r)
            else
               stabL (psi, fresh, PosAtom f :: gamma, delta, r)
          | F.Tensor (a,b) =>
            let
               val la = F.pos F.label a
               val lb = F.pos F.label b
               val (seqs, recon) = stabL (psi, fresh, gamma, a :: b :: delta, r)
            in
               (seqs, P.mapr (fn d => SC.TensorL (lf, (la, lb), d)) recon)
            end
          | F.One =>
            let
               val (seqs, recon) = stabL (psi, fresh, gamma, delta, r)
            in
               (seqs, P.mapr (fn d => SC.OneL (lf, d)) recon)
            end
          | F.Sum (a,b) =>
            let
               val la = F.pos F.label a
               val lb = F.pos F.label b
               val (seqs1, recon1) = stabL (psi, fresh, gamma, a :: delta, r)
               val (seqs2, recon2) = stabL (psi, fresh, gamma, b :: delta, r)
            in
               ( seqs1 @ seqs2
               , P.mapr2 (fn (d,d') => SC.PlusL (lf, (la,d), (lb,d')))
                    (recon1,recon2))
            end
          | F.Zero => ([], P.Leaf (SC.ZeroL lf))
          | F.Down a =>
            let
               val la = F.neg F.label a
               val (seqs, recon) = stabL (psi, fresh, Neg a :: gamma, delta, r)
            in
               (seqs, P.mapr (fn d => SC.DownL (lf, (la, d))) recon)
            end
          | F.Ex((x, _), a) =>
            let
               val p = Param.next ()
               val a = F.pos (F.apply1 (x, Term.Param p)) a
               val la = F.pos F.label a
               val fresh = Param.Set.add(fresh, p)
               val (seqs, recon) = stabL (psi, fresh, gamma, a :: delta, r)
            in
               (seqs, P.mapr (fn d => SC.ExL(lf, (la, p), d)) recon)
            end
      end

   val rec stabR
      :  C.t * Param.set * Left.t list * F.pos list * F.neg
      -> CSeq.t list * P.t = fn
      (psi, fresh, gamma, delta, f) =>
      case F.neg F.expose f of
         F.NAtom _ => stabL (psi, fresh, gamma, delta, NegAtom f)
       | F.With (a, b) =>
         let
            val (hyps1, recon1) = stabR (psi, fresh, gamma, delta, a)
            val (hyps2, recon2) = stabR (psi, fresh, gamma, delta, b)
         in
            (hyps1 @ hyps2, P.mapr2 SC.WithR (recon1, recon2))
         end
       | F.Top => ([], P.Leaf SC.TopR)
       | F.Lolli (a, b) =>
         let
            val la = F.pos F.label a
            val (hyps, recon) = stabR (psi, fresh, gamma, a :: delta, b)
         in
            (hyps, P.mapr (fn d => SC.LolliR (la, d)) recon)
         end
       | F.Up a =>
         let
            val (qs, recon) = stabL (psi, fresh, gamma, delta, Pos a)
         in
            (qs, P.mapr SC.UpR recon)
         end
       | F.BiLolli (a,b) =>
         if !Parameters.Focus.useBiimpContinue then
            let
               (* The case analysis searching for arrows makes a big difference.
                  e.g. a factor of 10 on SYJ006 *)
               val a' = a
               val b' = b
               (*  Freshening didn't work at all *)
               (* val a' = F.freshen a *)
               (* val b' = F.freshen b *)
               fun lab f = case F.neg F.expose f of
                  F.Up f => F.pos F.label f
                | _ => F.neg F.label f
               val (gamma1, delta1) = case F.neg F.expose a of
                  F.Up a => (gamma, a :: delta)
                | _ => (Neg a :: gamma, delta)
               val (hyps1, recon1) = stabR (psi, fresh, gamma1, delta1, b)
               val (gamma2, delta2) = case F.neg F.expose b' of
                  F.Up b' => (gamma, b' :: delta)
                | _ => (Neg b' :: gamma, delta)
               val (hyps2, recon2) = stabR (psi, fresh, gamma2, delta2, a')
               fun rleft r = case F.neg F.expose a of
                  F.Up _ => SC.UpR r
                | _ => r
               fun rright r = case F.neg F.expose b' of
                  F.Up _ => SC.UpR r
                | _ => r
               fun rfun (d1, d2) =
                  SC.BiLolliR ((lab a, rleft d1), (lab b', rright d2))
            in
               (hyps1 @ hyps2, P.mapr2 rfun (recon1, recon2))
            end
         else
            let
               val la = F.neg F.label a
               val lb = F.neg F.label b
               val (hyps1, recon1) =
                  stabR (psi, fresh, Neg a :: gamma, delta, b)
               val (hyps2, recon2) =
                  stabR (psi, fresh, Neg b :: gamma, delta, a)
               fun rfun (d1, d2) = SC.BiLolliR ((la, d1), (lb, d2))
            in
               (hyps1 @ hyps2, P.mapr2 rfun (recon1, recon2))
            end
       | F.All ((x, _), a) =>
         let
            val p = Param.next ()
            val fresh = PSet.add(fresh, p)
            val a = F.neg (F.apply1 (x, Term.Param p)) a
            val (qs, recon) = stabR (psi, fresh, gamma, delta, a)
         in
            (qs, P.mapr (fn d => SC.AllR(p, d)) recon)
         end

   val stabilize: PFormula.neg -> Stable.t = fn f =>
      let
         (* val _ = Log.trace (fn () => $"-> Focus.stabilize") *)
         val prop = F'.propositional f
         (* Label the formula. *)
         val lf = LFormula.make f
         val (seqs, proof) = stabR (C.Top, PSet.empty, [], [], lf)
         val subs = Sublabels.make lf
         val conflicts = Conflicts.make (subs, lf)
         val seqs = if prop then seqs else map CSeq.freeze seqs
         val {pos, neg} = F'.neg F'.preds f
         val preds = Pred.Set.union (pos, neg)
         val _ = Log.warning (fn () => F.ppLabels lf)
         (* ; Log.info (fn () => Sublabels.pp subs) *)
         val res = Stable.T
                      { formula = f
                      , lformula = lf
                      , seqs = seqs
                      , bipolarPreds = preds
                      , conflicts = conflicts
                      , proof = proof }
      in
         (* Log.trace (fn () => $"<- Focus.stabilize"); *)
         res
      end

   val genRules : Pred.set * Side.t list -> Rule.t' list =
      let
         datatype occ = POS | NEG
         val sideOcc = fn
            Left _ => NEG
          | Right _ => POS
         val sideEq = fn
            ((SOME l1, o1), (SOME l2, o2)) => Rel.eq (l1, l2) andalso o1 = o2
          | ((NONE, o1), (NONE, o2)) => o1 = o2
          | _ => false
         val rec gen
            :  Pred.set
            *  Side.t list
            *  (Rel.t option * occ) list
            *  Rule.t' list
            -> Rule.t' list = fn
            (_, [], _, acc) => acc
          | (preds, t :: ts, done, acc) =>
            let
               val id = (Side.label t, sideOcc t)
            in
               (* If we've already focused on an atom or logical constant,
                  continue. *)
               if List.genMem sideEq (id, done)
               then gen (preds, ts, done, acc)
               else (* otherwise focus *)
                  let
                     val rules = case t of
                        Right r => focusR (preds, r)
                      | Left l => focusL (preds, l)
                     fun newHypTargets (ants, cons) =
                        (Right cons :: map Left ants)
                     fun newHypsTargets hyps =
                        List.concat (map newHypTargets hyps)
                     val newTargets =
                        List.concat (map (newHypsTargets o #1) rules)
                  in
                     gen (preds, newTargets @ ts, id :: done, rules @ acc)
                  end
            end
      in
         fn (preds, targets) => gen (preds, targets, [], [])
      end

   val initial: {seq : CSeq.t, include_inconsistent_seq : bool} -> Foci.t = fn
      { seq = CSeq.T { seq = goal as (ants, r), constr, ...},
        include_inconsistent_seq } =>
      let
         (* Because the input is universally quantified and params are frozen,
            the goal should have no free variables or parameters.  Thus we
            don't need to rename the goal.  *)
         val _  = assert
                     (fn () => Atoms.isEmpty (Seq.atoms goal),
                      fn () => %[$"atoms in goal: ", Atoms.pp (Seq.atoms goal)])
         (* Determine the atoms occurring both positively and negatively. *)
         val {pos, neg} = F'.neg F'.preds (Seq.pformula goal)
         val preds = Pred.Set.intersection (pos, neg)
         (* Generate initial rules by focusing *)
         val rules =
            let
               (* Initial focusing targets *)
               val targets = Right r :: map Left ants
            in
               genRules (preds, targets)
            end
         (* Globalize *)
         val global = ants
         (* Add the inconsistent rule filter ⊥ | · ⊢ · *)
         val inconsis =
            Rule.T { hyps = [], concl = ([], Right.Var)
                   , fresh = PSet.empty, constr = C.Bot
                   , proof = P.Leaf SC.Hole }
         val (goal, rules) =
            let
               val global' = map Left.label global
               val goal = Seq.globalize global' goal
               val rules = map (Rule.globalize global') rules
            in
               (goal, rules)
            end
         val rules = map Rule.finish rules
         val rules =
            if include_inconsistent_seq then inconsis :: rules else rules
      in
         asserts (fn () => Atoms.isEmpty (Ants.atoms global), "global vars")
       ; Foci.T
            { rules = rules
            , goal = goal
            , global = global
            , constr = C.simplify constr }
      end
end
