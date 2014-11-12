
structure ND :> ND = struct

   open General
   open PP.Ops

   structure F = Formula
   structure U = Unicode
   structure S = Subst

   datatype intro =
      Pair of intro * intro
    | Lam of Label.t * intro
    | Inl of intro
    | Inr of intro
    | Case of elim
      * (Label.t * intro)
      * (Label.t * intro)
    | Unit
    | Abort of elim
    | Elim of elim
    | Let of (Label.t * elim) * intro
    | QLam of Var.t * intro
    | Pack of Term.t * intro
    | Unpack of Var.t * Label.t * elim * intro
    | Hole

   and elim =
      Label of Label.t
    | Fst of elim
    | Snd of elim
    | App of elim * intro
    | Ascribe of intro * Formula.t
    | QApp of elim * Term.t

   type t = intro

   val rec eqi = fn
      (Pair (i1, i2), Pair (i1', i2')) => eqi (i1, i1') andalso eqi (i2, i2')
    | (Lam (l, i), Lam (l', i')) => Label.eq (l, l') andalso eqi (i, i')
    | (Inl i, Inl i') => eqi (i, i')
    | (Inr i, Inr i') => eqi (i, i')
    | (Case (e1, (l1, i1), (l2, i2)), Case (e1', (l1', i1'), (l2', i2'))) =>
      eqe (e1, e1') andalso Label.eq (l1, l1') andalso eqi (i1, i1')
      andalso Label.eq (l2, l2') andalso eqi (i2, i2')
    | (Unit, Unit) => true
    | (Abort e, Abort e') => eqe (e, e')
    | (Elim e, Elim e') => eqe (e, e')
    | (Let ((l, e1), e2), Let ((l', e1'), e2')) =>
      Label.eq (l, l') andalso eqe (e1, e1') andalso eqi (e2, e2')
    | (QLam (l, i), QLam (l', i')) => Var.eq (l, l') andalso eqi (i, i')
    | (Pack (l, i), Pack (l', i')) => Term.eq (l, l') andalso eqi (i, i')
    | (Unpack (v, l, e, i), Unpack (v', l', e', i')) =>
      Var.eq (v, v') andalso Label.eq (l, l') andalso
      eqe (e, e') andalso eqi (i, i')
    | _ => false
   and eqe = fn
      (Label l, Label l') => Label.eq (l, l')
    | (Fst e, Fst e') => eqe (e, e')
    | (Snd e, Snd e') => eqe (e, e')
    | (App (e, i), App (e', i')) => eqe (e, e') andalso eqi (i, i')
    | (Ascribe (i, f), Ascribe (i', f')) =>
      eqi (i, i') andalso Formula.eq (f, f')
    | (QApp (l, i), QApp (l', i')) => eqe (l, l') andalso Term.eq (i, i')
    | _ => false

   local
      (* operator precedences *)
      val atomPrec = 30
      val callPrec = 20
      val funPrec = 15
      val letPrec = 10
      val weakestPrec = 0

      val precE = fn
         Label _ => atomPrec
       | Fst _ => callPrec
       | Snd _ => callPrec
       | App _ => callPrec
       | Ascribe _ => weakestPrec
       | QApp _ => callPrec

      val precI = fn
         Pair _ => atomPrec
       | Lam _ => funPrec
       | Inl _ => callPrec
       | Inr _ => callPrec
       | Case _ => letPrec
       | Unit => atomPrec
       | Abort _ => callPrec
       | Elim e => precE e
       | Let _ => letPrec
       | QLam _ => funPrec
       | Pack _ => letPrec
       | Unpack _ => letPrec
       | Hole => atomPrec

      val precT = fn
         Term.Var _ => atomPrec
       | Term.Param _ => atomPrec
       | Term.Fn (_, []) => atomPrec
       | Term.Fn (_, _) => callPrec

      fun ` x = Label.pp x
      fun `` x = Var.pp x
   in
      val rec pp = fn
         Label l => `l
       | Fst t =>
         let
            val p = pp t
            val p = if precE t <= callPrec then PP.paren p else p
         in
            %%[$"fst", p]
         end
       | Snd t =>
         let
            val p = pp t
            val p = if precE t <= callPrec then PP.paren p else p
         in
            %%[$"snd", p]
         end
       | App (t1, t2) =>
         let
            val p = pp t1
            val q = ppi t2
            val p = if precE t1 <= callPrec then PP.paren p else p
            val q = if precI t2 <= callPrec then PP.paren q else q
         in
            PP.hang p 2 q
         end
       | Ascribe (t, f) => PP.paren (%[ppi t, $":", F.pp f])
       | QApp (t1, t2) =>
         let
            val p = pp t1
            val q = Term.pp t2
            val p = if precE t1 <= callPrec then PP.paren p else p
            val q = if precT t2 <= callPrec then PP.paren q else q
         in
            PP.hang p 0 q
         end

      and ppi = fn
         Pair (t1, t2) => %[$"(", PP.hang (%[ppi t1, $","]) 0 (%[ppi t2, $")"])]
       | Lam (x, t) => PP.hang (%[$U.lambda, `x, $"."]) 0 (ppi t)
       | Inl t =>
         let
            val p = ppi t
            val p = if precI t <= callPrec then PP.paren p else p
         in
            %[$"inl", p]
         end
       | Inr t =>
         let
            val p = ppi t
            val p = if precI t <= callPrec then PP.paren p else p
         in
            %[$"inr", p]
         end
       | Case (t, (l1, t1), (l2, t2)) =>
         &[%[$"case ", pp t, $" of "],
           %[\\, &[ %[$"   Inl ", `l1, $" => ", ppi t1],
                   %[$" | Inr ", `l2, $" => ", ppi t2]]]]
       | Unit => $"()"
       | Abort t =>
         let
            val p = pp t
            val p = if precE t <= callPrec then PP.paren p else p
         in
            PP.hang ($"abort") 0 p
         end
       | Elim e => pp e
       | Let ((x, t1), t2) =>
         &[ PP.hang (%%[$"let", `x, $"="]) 2 (%%[pp t1, $"in"])
          , ppi t2 ]
       | QLam (a, t) => PP.hang (%[$U.lambda, ``a, $"."]) 2 (ppi t)
       | Pack (t1, t2) =>
         PP.hang (%[$"pack ", $U.langle, Term.pp t1, $","]) 2
            (%[ppi t2, $U.rangle])
       | Unpack (a, u, e, i) =>
         &[ PP.hang (%[ $"unpack ", $U.langle, ``a, $", ", `u , $U.rangle,
                       $" ="]) 2 (%[pp e, $" in "])
          , %[\\, ppi i]
          , $"end" ]
       | Hole => $U.circ

      val pp = ppi
   end

   val fv : t -> Var.set =
      let
         val rec fvI = fn
            Pair (I1, I2) => Var.Set.union (fvI I1, fvI I2)
          | Lam (_, I) => fvI I
          | Inl I => fvI I
          | Inr I => fvI I
          | Case (E, (_, I1), (_, I2)) => Var.Set.unions [fvI I1, fvI I2, fvE E]
          | Unit => Var.Set.empty
          | Abort E => fvE E
          | Elim E => fvE E
          | Let ((_, E), I) => Var.Set.union (fvE E, fvI I)
          | QLam (x, I) => Var.Set.deleteIfMem (fvI I, x)
          | Pack (t, I) => Var.Set.union (Term.vars t, fvI I)
          | Unpack (x, _, E, I) =>
            Var.Set.deleteIfMem (Var.Set.union (fvE E, fvI I), x)
          | Hole => Var.Set.empty
         and fvE = fn
            Label _ => Var.Set.empty
          | Fst E => fvE E
          | Snd E => fvE E
          | App (E, I) => Var.Set.union (fvE E, fvI I)
          | Ascribe (I, _) => fvI I
          | QApp (E, t) => Var.Set.union (fvE E, Term.vars t)
      in
         fvI
      end

   fun renameI (f, xy as (x, y)) = case f of
      Pair (i1, i2) => Pair (renameI (i1, xy), renameI (i2, xy))
    | Lam (u, i) => Lam (u, renameI (i, xy))
    | Inl i => Inl (renameI (i, xy))
    | Inr i => Inr (renameI (i, xy))
    | Case (e, (u, i1), (v, i2)) =>
      Case (renameE (e, xy), (u, renameI (i1, xy)), (v, renameI (i2, xy)))
    | Unit => Unit
    | Abort e => Abort (renameE (e, xy))
    | QLam (x', i) => if Var.eq (x, x') then f else QLam (x', renameI (i, xy))
    | Pack (t, i) => Pack (Term.apply1 (t, (x, Term.Var y)), renameI (i, xy))
    | Unpack (x', u, e, i) =>
      if Var.eq (x, x') then f
      else Unpack (x', u, renameE (e, xy), renameI (i, xy))
    | Elim e => Elim (renameE (e, xy))
    | Let ((u, e), i) => Let ((u, renameE (e, xy)), renameI (i, xy))
    | Hole => Hole

   and renameE (f, xy as (x, y)) = case f of
      Label _ => f
    | Fst e => Fst (renameE (e, xy))
    | Snd e => Snd (renameE (e, xy))
    | App (e, i) => App (renameE (e, xy), renameI (i, xy))
    | QApp (e, t) => QApp (renameE (e, xy), Term.apply1 (t, (x, Term.Var y)))
    | Ascribe (i, f) => Ascribe (renameI (i, xy), f)

   val apply1 =
      let
         fun substI (f, xt as (x, t)) = case f of
            Pair (i1, i2) => Pair (substI (i1, xt), substI (i2, xt))
          | Lam (u, i) => Lam (u, substI (i, xt))
          | Inl i => Inl (substI (i, xt))
          | Inr i => Inr (substI (i, xt))
          | Case (e, (u, i1), (v, i2)) =>
            Case (substE (e, xt), (u, substI (i1, xt)), (v, substI (i2, xt)))
          | Unit => Unit
          | Abort e => Abort (substE (e, xt))
          | QLam (x', i) =>
            if Var.eq (x, x') then f
            else if Var.Set.mem (Term.vars t, x') then
               let
                  val v = Var.next ()
                  val i' = renameI (i, (x', v))
               in
                  QLam (v, substI (i', (x, t)))
               end
            else QLam (x', substI (i, (x, t)))
          | Pack (t', i) => Pack (Term.apply1 (t', xt), substI (i, xt))
          | Unpack (x', u, e, i) =>
            if Var.eq (x, x')
            then f
            else if Var.Set.mem (Term.vars t, x') then
               let
                  val v = Var.next ()
                  val e' = renameE (e, (x', v))
                  val i' = renameI (i, (x', v))
               in
                  Unpack (v, u, substE (e', xt), substI (i', xt))
               end
            else Unpack (x', u, substE (e, xt), substI (i, xt))
          | Elim e => Elim (substE (e, xt))
          | Let ((u, e), i) => Let ((u, substE (e, xt)), substI (i, xt))
          | Hole => Hole
         and substE (f, xt) = case f of
            Label _ => f
          | Fst e => Fst (substE (e, xt))
          | Snd e => Snd (substE (e, xt))
          | App (e,i) => App (substE (e, xt), substI (i, xt))
          | QApp (e,t') => QApp (substE (e, xt), Term.apply1 (t', xt))
          | Ascribe (i, f) => Ascribe (substI (i, xt), f)
      in
         substI
      end

   val paramSubst : intro * (Param.t * Var.t) -> intro =
      let
         fun psubI (f, ax as (_, x)) = case f of
            Pair (i1, i2) => Pair (psubI (i1, ax), psubI (i2, ax))
          | Lam (u, i) => Lam (u, psubI (i, ax))
          | Inl i => Inl (psubI (i, ax))
          | Inr i => Inr (psubI (i, ax))
          | Case (e, (u, i1), (v, i2)) =>
            Case (psubE (e, ax), (u, psubI (i1, ax)), (v, psubI (i2, ax)))
          | Unit => Unit
          | Abort e => Abort (psubE (e, ax))
          | QLam (x', i) =>
            if Var.eq (x, x')
            then let
               val v = Var.next ()
               val i' = renameI (i, (x', v))
            in
               QLam (v, psubI (i', ax))
            end
            else QLam (x', psubI (i, ax))
          | Pack (t, i) => Pack (Term.paramSubst (t, ax), psubI (i, ax))
          | Unpack (x', u, e, i) =>
            if Var.eq (x, x')
            then let
               val v = Var.next ()
               val e' = renameE (e, (x', v))
               val i' = renameI (i, (x', v))
            in
               Unpack (v, u, psubE (e', ax), psubI (i', ax))
            end
            else Unpack (x', u, psubE (e, ax), psubI (i, ax))
          | Elim e => Elim (psubE (e, ax))
          | Let ((u, e), i) => Let ((u, psubE (e, ax)), psubI (i, ax))
          | Hole => Hole
         and psubE (f, ax) = case f of
            Label _ => f
          | Fst e => Fst (psubE (e, ax))
          | Snd e => Snd (psubE (e, ax))
          | App (e,i) => App (psubE (e, ax), psubI (i, ax))
          | QApp (e,t) => QApp (psubE (e, ax), Term.paramSubst (t, ax))
          | Ascribe (i, f) => Ascribe (psubI (i, ax), f)
      in
         psubI
      end

   exception Check

   type ctx = (Label.t * Formula.t) list
   fun ppCtx (l:ctx) = &(map (fn (l, f) => PP.pair (Label.pp l, Formula.pp f)) l)

   fun check { eq, ctx, term, form } =
      let
         (* Remove labels before checking proof. *)
         val form = F.unlabel form
         fun chk ctx = fn
            (Pair (t1, t2), F.And (p, q)) =>
            let in
               chk ctx (t1, p)
             ; chk ctx (t2, q)
            end
          | (Pair (t1, t2), F.Iff (p, q)) =>
            let in
               chk ctx (t1, F.Imp (p, q))
             ; chk ctx (t2, F.Imp (q, p))
            end
          | (Lam (x, t), F.Imp (p, q)) => chk ((x, p) :: ctx) (t, q)
          | (Lam (x, t), F.Not p) => chk ((x, p) :: ctx) (t, F.Bot)
          | (Inl t, F.Or (p, _)) => chk ctx (t, p)
          | (Inr t, F.Or (_, q)) => chk ctx (t, q)
          | (Case (c, (x, t1), (y, t2)), r) =>
            let
               open PP.Ops
               val res = syn ctx c
            in
               case res of
                  F.Or (p, q) =>
                  let in
                     chk ((x, p)::ctx) (t1, r)
                   ; chk ((y, q)::ctx) (t2, r)
                  end
                | _ => (PP.ppl (%[$"Case expression: ", F.pp res]); raise Check)
            end
          | (Unit, F.Top) => ()
          | (Abort t, _) =>
            if F.eq (syn ctx t, F.Bot) then ()
            else (printl "check abort"; raise Check)
          | (Elim t, p) =>
            let
               val p' = syn ctx t
            in
               if F.eq' eq (p, p') then () else
               let in
                  PP.ppl (&[ %[$"Expected: ", F.pp p]
                           , %[$"Found   : ", F.pp p']])
                ; raise Check
               end
            end
          | (Let ((x, t1), t2), p) =>
            chk ((x, syn ctx t1) :: ctx) (t2, p)
          | (QLam (x, i), F.All ((x', _), A)) =>
            let
               val a = Term.Param (Param.next ())
            in
               chk ctx (apply1 (i, (x, a)), F.apply1 (A, (x', a)))
            end
          | (Pack (t, i), F.Ex ((x, _), A)) =>
            chk ctx (i, F.apply1 (A, (x, t)))
          | (Unpack (x, v, E, I), C) =>
            let in
               case syn ctx E of
                  F.Ex ((x', _), A) =>
                  let
                     val a = Term.Param (Param.next ())
                     val ctx' = (v, F.apply1 (A, (x', a))) :: ctx
                     val I' = apply1 (I, (x, a))
                  in
                     chk ctx' (I', C)
                  end
                | _ => (printl "check unpack"; raise Check)
            end
          | (Hole, _) => ()
          | _ => raise Check
         and syn ctx = fn
            Label x =>
            let in
               case List.genAssoc Label.eq (x, ctx) of
                  SOME p => p
                | NONE =>
                  let in
                     PP.ppl (&[ %[$"Can't find label:", Label.pp x]
                              , %[ppCtx ctx]])
                   ; raise Check
                  end
            end
          | Fst t =>
            let in
               case syn ctx t of
                  F.And (p, _) => p
                | F.Iff (p, q) => F.Imp (p, q)
                | _ => (printl "check fst"; raise Check)
            end
          | Snd t =>
            let in
               case syn ctx t of
                  F.And (_, q) => q
                | F.Iff (p, q) => F.Imp (q, p)
                | _ => (printl "check snd"; raise Check)
            end
          | App (t1, t2) =>
            let in
               case syn ctx t1 of
                  F.Imp (p, q) =>
                  let in
                     chk ctx (t2, p)
                   ; q
                  end
                | _ => (printl "check app"; raise Check)
            end
          | Ascribe (t, p) =>
            let in
               chk ctx (t, p)
             ; p
            end
          | QApp (e, t) =>
            let in
               case syn ctx e of
                  F.All ((x, _), A) => F.apply1 (A, (x, t))
                | _ => (printl "check qapp"; raise Check)
            end
      in
         let in
            (* Log.trace (fn () => *)
            (*    &[ $"Checking term: ", %[\\, pp nd] *)
            (*     , $"Against form : ", %[\\, F.pp f] *)
            (*     , $"in context   : ", *)
            (*       %[\\, &(map (fn (l, f) => %[Label.pp l, $": ", F.pp f]) ctx)]]); *)
            chk ctx (term, form)
         end
      end

   (*** Normalization ***)

   (* Normal forms I : intro have no occurrence of Ascribe or Let
      Write I nf if I is in normal form and nf (I) for a normal form of I

      Substitutions S : var -> intro
      Invariants: S is total, S (x) nf for all variables x *)

   structure Ctx :>
      sig
         type t
         val extendV: t -> Var.t * Term.t -> t
         val extendL: t -> Label.t * intro -> t
         val applyT: t -> Term.t -> Term.t
         val applyL: t -> Label.t -> intro
         val id: t
      end =
      struct
      type t = intro Label.Map.t * Subst.t
      fun extendV (m, s) (v, t) = (m, S.extend (s, Left (v, t)))
      (* We use Map.replace because labels can be shawdowed by labels of the
         same name. *)
      fun extendL (m, s) (l, i) = (Label.Map.replace (m, l, i), s)
      fun applyT (_, s) t = S.apply (t, s)
      fun applyL (m, _) l = case Label.Map.find (m, l) of
         SOME i => i
       | NONE =>
         let in
            printl ("can't find label: " ^ Label.toString l)
          ; Elim (Label Label.bug)
         end
      val id: t = (Label.Map.empty, S.id)
   end

   (* normi ctx I = I' where I' = nf (I[ctx])
      norme ctx E = I' where I' = nf (E[ctx])
      normb ctx (x,I) = (x',I') where I' = nf (I[ctx+[x->x'])), x' fresh *)

   val rec normi : Ctx.t -> intro -> intro =
      fn ctx =>
      fn Pair (I1,I2) => Pair (normi ctx I1, normi ctx I2)
       | Lam (u,I) => Lam (normb ctx (u,I))
       | Inl I1 => Inl (normi ctx I1)
       | Inr I2 => Inr (normi ctx I2)
       | Case (E, (u1,I1), (u2,I2)) =>
         case_ (norme ctx E, normb ctx (u1,I1), normb ctx (u2,I2))
       | Unit => Unit
       | Abort E => abort_ (norme ctx E)
       | Elim E => norme ctx E
       | Let ((x,E), I) => normi (Ctx.extendL ctx (x, norme ctx E)) I
       | QLam (x,I) => QLam (normv ctx (x, I))
       | Pack (t,I) => Pack (Ctx.applyT ctx t, normi ctx I)
       | Unpack (x, u, E, I) => normup ctx (x, u, norme ctx E, I)
       | Hole => Hole

   and normup =
      fn ctx =>
      fn (x, u, E, I) =>
         let
            val x' = Var.next ()
            val u' = Label.next ()
            val ctx = Ctx.extendL ctx (u, Elim (Label u'))
            val ctx = Ctx.extendV ctx (x, Term.Var x')
         in
            unpack_ ctx (x', u', E, I)
         end

   and norme : Ctx.t -> elim -> intro =
      fn ctx => fn e => case e of
         Label u => Ctx.applyL ctx u (* normal by invariant on ctx *)
       | Fst E => fst_ (norme ctx E)
       | Snd E => snd_ (norme ctx E)
       | App (E,I) => app_ (norme ctx E, normi ctx I)
       | Ascribe (I,_) => normi ctx I
       | QApp (E, t) => qapp_ (norme ctx E, Ctx.applyT ctx t)

   and normb : Ctx.t -> Label.t * intro -> Label.t * intro =
      fn ctx =>
      fn (u,I) =>
         let
            val u' = Label.next ()
            val ctx' = Ctx.extendL ctx (u, Elim (Label u'))
         in
            (u', normi ctx' I)
         end

   and normv =
      fn ctx =>
      fn (x,I) =>
         let
            val x' = Var.next ()
            val ctx' = Ctx.extendV ctx (x, Term.Var x')
         in
            (x', normi ctx' I)
         end

   (* fst_ (I) = nf (fst (I)), assuming I:A&B nf *)
   and fst_ : intro -> intro = fn
      Pair (I1,_) => I1
    | Case (E,(y1,J1),(y2,J2)) => Case (E,(y1,fst_ J1), (y2, fst_ J2))
    | Abort (E) => Abort (E)
    | Elim (E) => Elim (Fst (E))
    | _ => raise Check (* term must be ill-typed *)

   (* snd_ (I) = nf (snd (I)), assuming I:A&B nf *)
   and snd_ : intro -> intro = fn
      Pair (_,I2) => I2
    | Case (E,(y1,J1),(y2,J2)) => Case (E,(y1, snd_ J1), (y2, snd_ J2))
    | Abort (E) => Abort (E)
    | Elim (E) => Elim (Snd (E))
    | _ => raise Check (* term must be ill-typed *)

   (* app_ (I1, I2) = nf (app (I1,I2)), assuming I1:A=>B, I2:A nf *)
   and app_ : intro * intro -> intro = fn
      (Lam (u2,I1), I2) => normi (Ctx.extendL Ctx.id (u2,I2)) I1
    | (Case (E,(y1,J1),(y2,J2)), I2) => Case (E, (y1,app_(J1,I2)), (y2,app_(J2,I2)))
    | (Abort E, _) => Abort (E)
    | (Elim E1, I2) => Elim (App (E1, I2))
    | _ => raise Check (* term must be ill-typed *)

   and qapp_ = fn
      (QLam (x,I), t) =>
      let
         val ctx = Ctx.extendV Ctx.id (x, t)
      in
         normi ctx I
      end
    | (Case (E,(y1,J1),(y2,J2)), I2) =>
      Case (E, (y1,qapp_(J1,I2)), (y2,qapp_(J2,I2)))
    | (Abort E, _) => Abort (E)
    | (Elim E1, I2) => Elim (QApp (E1, I2))
    | _ => raise Check (* term must be ill-typed *)

   (* case_ (I, (x1,I1), (x2,I2)) = nf (case (I,(x1,I1),(x2,I2))),
      assuming I:A|B, (x1,I1):A.C, (x2,I2):B.C nf *)
   and case_ : intro * (Label.t * intro) * (Label.t * intro) -> intro = fn
      (Inl I, (u1,I1), _) => normi (Ctx.extendL Ctx.id (u1,I)) I1
    | (Inr I, _, (u2,I2)) => normi (Ctx.extendL Ctx.id (u2,I)) I2
    | (Case (E, (y1,J1), (y2,J2)), (x1, I1), (x2, I2)) =>
      Case (E, (y1, case_ (J1, (x1,I1), (x2,I2))), (y2, case_ (J2, (x1,I1), (x2,I2))))
    | (Abort E, _, _) => Abort E
    | (Elim E, (x1,I1), (x2,I2)) => Case (E, (x1, I1), (x2, I2))
    | (Unpack (x, u, E, I), (u1, I1), (u2, I2)) =>
      Unpack (x, u, E, case_ (I, (u1, I1), (u2, I2)))
    | _ => raise Check (* term must be ill-typed *)

   (* abort_ (I) = nf (abort (I)), assuming I:F nf *)
   and abort_ : intro -> intro = fn
      Abort (E) => Abort (E)
    | Case (E, (y1,J1), (y2,J2)) => Case (E, (y1, abort_ J1), (y2, abort_ J2))
    | Elim (E) => Abort (E)
    | _ => raise Check (* term must be ill-typed *)

   and unpack_ =
      fn ctx =>
      fn (x, u1, Pack (t, I1), I) =>
         let
            val ctx = Ctx.extendV ctx (x, t)
            val ctx = Ctx.extendL ctx (u1, I1)
         in
            normi ctx I
         end
       | (x, u, Case (E, (v1, J1), (v2, J2)), I) =>
         Case (E,
               (v1, unpack_ ctx (x, u, J1, I)),
               (v2, unpack_ ctx (x, u, J2, I)))
       | (_, _, Abort E, _) => Abort E
       | (x, u, Elim E, I) =>
         let
            val ctx = Ctx.extendV ctx (x, Term.Var x)
            val ctx = Ctx.extendL ctx (u, Elim (Label u))
            val I' = normi ctx I
         in
            Unpack (x, u, E, I')
         end
       | _ => raise Check

   (* replace free variables with a dummy constant *)
   fun dummy () = Term.Fn (Func.ofString "c", [])

   fun ground t =
      let
         val fvs = Var.Set.toList (fv t)
      in
         foldr (fn (x, I) => apply1 (I, (x, dummy ()))) t fvs
      end

   fun normalize t =
      let in
         (* Start off the counting from 0 for prettier proofs.
            Reset the func table so 'dummy' will work. *)
         (* Func.reset (); *)
         (* Sym.reset (); *)
         ground (normi Ctx.id t)
      end

   val rec size = fn
      Pair (p1, p2) => 1 + size p1 + size p2
    | Lam (_, p) => 1 + size p
    | Inl p => 1 + size p
    | Inr p => 1 + size p
    | Case (p, (_, q), (_, r)) => 1 + size' p + size q + size r
    | Unit => 1
    | Abort e => 1 + size' e
    | Elim e => 1 + size' e
    | Let ((_, e), p) => 1 + size' e + size p
    | QLam (_, i) => 1 + size i
    | Pack (_, i) => 1 + size i
    | Unpack (_, _, e, i) => 1 + size' e + size i
    | Hole => 1
   and size' = fn
      Label _ => 1
    | Fst e => 1 + size' e
    | Snd e => 1 + size' e
    | App (e, i) => 1 + size' e + size i
    | Ascribe (i, _) => 1 + size i
    | QApp (e, _) => 1 + size' e


   (* Assumes bound variable names are unique. *)
   local
      fun makeMap m = fn
         (Lam (l, p), F.Imp (F.Label (s, a), b)) =>
         let
            val s = Label.ofString s
            val m = Label.Map.replace(m, l, s)
         in
            makeMap m (p, b)
         end
       | (Lam (_, p), F.Imp (_, b)) => makeMap m (p, b)
       | _ => m
   in
      fun label (nd, f) =
         let
            val m = makeMap Label.Map.empty (nd, f)
            fun get l =
               case Label.Map.find (m, l) of
                  NONE => l
                | SOME l => l
            fun elim f = case f of
               Label l => Label (get l)
             | Fst a => Fst (elim a)
             | Snd a => Snd (elim a)
             | App (a, b) => App (elim a, intro b)
             | Ascribe (a, t) => Ascribe (intro a, t)
             | QApp (a, t) => QApp (elim a, t)
            and intro f = case f of
               Lam (l, a) => Lam (get l, intro a)
             | Pair (a, b) => Pair (intro a, intro b)
             | Inl a => Inl (intro a)
             | Inr a => Inr (intro a)
             | Case (e, (l1, a1), (l2, a2)) =>
               Case (elim e, (get l1, intro a1), (get l2, intro a2))
             | Unit => Unit
             | Abort e => Abort (elim e)
             | Elim e => Elim (elim e)
             | Let ((l, a), b) => Let ((get l, elim a), intro b)
             | QLam (x, a) => QLam (x, intro a)
             | Pack (t, a) => Pack (t, intro a)
             | Unpack (x, l, a, b) => Unpack (x, get l, elim a, intro b)
             | Hole => Hole
         in
            intro nd
         end
   end

end
