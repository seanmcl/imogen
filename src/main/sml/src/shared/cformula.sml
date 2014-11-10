
structure CFormula :> CFormula = struct
   structure U = Unicode

   open General
   open PP.Ops

   structure Export = struct
      datatype t =
         imogen.Atom of Rel.t
       | Top
       | Bot
       | imogen.And of t * t
       | imogen.Imp of Rel.t * t
       | All of (Var.t * Sort.t) * t
       | Ex of (Var.t * Sort.t) * t
       | Hole
   end
   datatype t = datatype Export.t
   type simp = t -> Subst.t * t
   fun noSimp t = (Subst.id, t)

   type printable = t
   type eqable = t
   type fixable = t

   val isBot = fn
      Bot => true
    | _ => false

   val listConj = fn
      [] => Top
    | l => List.foldr1 imogen.And l

   val rec conjuncts = fn
      imogen.And (a, b) => conjuncts a @ conjuncts b
    | f => [f]

   fun map f fm =
      let
         val rec g = fn
            imogen.Atom r => imogen.Atom (f r)
          | imogen.And (a, b) => imogen.And (g a, g b)
          | imogen.Imp (a, b) => imogen.Imp (f a, g b)
          | All (x, a) => All (x, g a)
          | Ex (x, a) => Ex (x, g a)
          | f => f
      in
         g fm
      end

   fun propositional f =
      let
         val rec g = fn
            imogen.And (a, b) => g a andalso g b
          | imogen.Imp (_, b) => g b
          | All _ => false
          | Ex _ => false
          | _ => true
      in
         g f
      end

   fun unfix f = map Rel.unfix f
   (* fun fix' ats f = map (Rel.fix' ats) f *)
   fun fix _ = raise Unimplemented

   fun paramSubst (f, av) = map (fn r => Rel.paramSubst (r, av)) f
   fun freeze f = map Rel.freeze f
   fun thaw s f = map (Rel.thaw s) f

   fun quantifyParams (f, ps) =
      let
         fun ffn (p, f) =
            let
               val x = Var.next ()
            in
               All ((x, Sort.Base.I), paramSubst (f, (p, x)))
            end
      in
         Param.Set.fold ffn f ps
      end

   fun quantifyVars (f, xs) =
      let
         fun ffn (x, f) = Ex ((x, Sort.Base.I), f)
      in
         Var.Set.fold ffn f xs
      end

   fun fill {fillee, filler} =
      let
         val rec f = fn
            Hole => (filler, true)
          | imogen.And (t1, t2) =>
            let
               val (t1, filled) = f t1
            in
               if filled then (imogen.And (t1, t2), true)
               else
                  let
                     val (t2, filled) = f t2
                  in
                     (imogen.And (t1, t2), filled)
                  end
            end
          | imogen.Imp (r, t) =>
            let
               val (t, filled) = f t
            in
               (imogen.Imp (r, t), filled)
            end
          | All (x, t) =>
            let
               val (t, filled) = f t
            in
               (All (x, t), filled)
            end
          | Ex (x, t) =>
            let
               val (t, filled) = f t
            in
               (Ex (x, t), filled)
            end
          | f => (f, false)
         val (c, filled) = f fillee
      in
         if filled then c else failwith "Hole not filled"
      end

   local
      fun veq ((x1, s1), (x2, s2)) =
         Var.eq (x1, x2) andalso Sort.Base.eq (s1, s2)
   in
      val rec eq = fn
         (imogen.Atom rel1, imogen.Atom rel2) =>
         let
            val (p, xs) = Rel.dest rel1
            val (q, ys) = Rel.dest rel2
         in
            Pred.eq (p, q) andalso List.all2 Term.eq (xs, ys)
         end
       | (Top, Top) => true
       | (Bot, Bot) => true
       | (imogen.And (x1, y1), imogen.And (x2, y2)) => eq (x1, x2) andalso eq (y1, y2)
       | (imogen.Imp (x1, y1), imogen.Imp (x2, y2)) => Rel.eq (x1, x2) andalso eq (y1, y2)
       | (All (x, f), All (y, g)) => veq (x, y) andalso eq (f, g)
       | (Ex (x, f), Ex (y, g)) => veq (x, y) andalso eq (f, g)
       | _ => false
   end

   val rec atoms = fn
      imogen.Atom rel => Rel.atoms rel
    | imogen.And (p, q) => Atoms.union (atoms p, atoms q)
    | imogen.Imp (r, q) => Atoms.union (Rel.atoms r, atoms q)
    | All ((x, _), p) => Atoms.remove (atoms p, Left x)
    | Ex ((x, _), p) => Atoms.remove (atoms p, Left x)
    | _ => Atoms.empty

   fun rename1 (x, y) =
      let
         val xy = (x, Term.Var y)
         fun rel r = Rel.apply1 (r, xy)
         fun f fm = case fm of
            imogen.Atom r => imogen.Atom (rel r)
          | All ((x', s), a) =>
            if Var.eq (x, x') then fm else All ((x', s), f a)
          | Ex ((x', s), a) =>
            if Var.eq (x, x') then fm else Ex ((x', s), f a)
          | imogen.And (p, q) => imogen.And (f p, f q)
          | imogen.Imp (r, q) => imogen.Imp (rel r, f q)
          | _ => fm
      in
         f
      end

   fun apply1 (f, xt as (x, t)) =
      let
         fun rel r = Rel.apply1 (r, xt)
         fun subst fm = case fm of
            imogen.Atom r => imogen.Atom (rel r)
          | All ((x', s), a) =>
            if Var.eq (x, x') then fm else
            if Atoms.mem (Term.atoms t, Left x') then
               let
                  val v = Var.next ()
                  val a' = rename1 (v, x') a
               in
                  All ((v, s), subst a')
               end
            else All ((x', s), subst a)
          | Ex ((x', s), a) =>
            if Var.eq (x, x') then fm else
            if Atoms.mem (Term.atoms t, Left x') then
               let
                  val v = Var.next ()
                  val a' = rename1 (v, x') a
               in
                  Ex ((v, s), subst a')
               end
            else Ex ((x', s), subst a)
          | imogen.And (p, q) => imogen.And (subst p, subst q)
          | imogen.Imp (r, q) => imogen.Imp (rel r, subst q)
          | _ => fm
      in
         subst f
      end

   (* ----------------------------------------------------------------------- *)
   (*  Printing                                                               *)
   (* ----------------------------------------------------------------------- *)

   local
      structure Prec = Parse.Prec

      val prec = fn
         imogen.Imp _ => Prec.imogen.Imp
       | imogen.And _ => Prec.imogen.And
       | All _ => Prec.Quant
       | Ex _ => Prec.Quant
       | _ => Prec.imogen.Atom

      fun destAll (All (x, b)) =
         let
            val (xs, b') = destAll b
         in
            (x::xs, b')
         end
        | destAll f = ([], f)

      fun destEx (Ex (x, b)) =
         let
            val (xs, b') = destEx b
         in
            (x::xs, b')
         end
        | destEx f = ([], f)
   in
      fun pp f =
         let
            val fprec = prec f
            fun var (x, s) = %[Var.pp x, $":", Sort.Base.pp s]
         in
            case f of
               imogen.Atom rel => Rel.pp rel
             | Top => $U.top
             | Bot => $U.bot
             | Hole => $U.circ
             | imogen.And (p, q) =>
               let
                  val p' = pp p
                  val p' = if prec p < fprec then PP.paren p' else p'
                  val q' = pp q
                  val q' = if prec q <= fprec then PP.paren q' else q'
               in
                  PP.hang (%[p', \, $U.wedge]) 2 q'
               end
             | imogen.Imp (r, q) =>
               let
                  val r = Rel.pp r
                  val q' = pp q
                  val q' = if prec q < fprec then PP.paren q' else q'
               in
                  PP.hang (%[r, \, $U.sup]) 2 q'
               end
             | f as All _ =>
               let
                  val (xs, p) = destAll f
                  val p' = pp p
                  val p' = if prec p < fprec then PP.paren p' else p'
               in
                  PP.hang (%[$U.all, \, %(PP.punctuate \ (List.map var xs)), $"."]) 2 p'
               end
             | f as Ex _ =>
               let
                  val (xs, p) = destEx f
                  val p' = pp p
                  val p' = if prec p < fprec then PP.paren p' else p'
               in
                  PP.hang (%[$U.exists, \, %(PP.punctuate \ (List.map var xs)), $"."]) 2 p'
               end
         end
   end (* local *)

   fun apply (f, s) =
      let
         fun sub s = fn
            imogen.Atom r => imogen.Atom (Rel.apply (r, s))
          | Top => Top
          | Bot => Bot
          | imogen.And (a, b) => imogen.And (sub s a, sub s b)
          | imogen.Imp (r, a) => imogen.Imp (Rel.apply (r, s), sub s a)
          | All f => quant All s f
          | Ex f => quant Ex s f
          | Hole => Hole
         and quant con s ((x, t), a) =
            let
               val s = Subst.remove (s, Left x)
            in
               if Atoms.mem (Subst.img s, Left x) then
                  let
                     val y = Var.next ()
                     val a = rename1 (x, y) a
                  in
                     con ((y, t), sub s a)
                  end
               else con ((x, t), sub s a)
            end
      in
         sub s f
      end

   val simplify =
      let
         fun quant f ((x, s), p) =
            if Atoms.mem (atoms p, Left x) then f ((x, s), p) else p
         val rec simp1 = fn
            imogen.And (a, Top) => a
          | imogen.And (Top, a) => a
          | imogen.And (Bot, _) => Bot
          | imogen.And (_, Bot) => Bot
          | imogen.Imp (_, Top) => Top
          | f => f
         val rec simp = fn
            imogen.And (p, q) => simp1 (imogen.And (simp p, simp q))
          | imogen.Imp (r, p) => simp1 (imogen.Imp (r, simp p))
          | All (x, p) => quant All (x, simp p)
          | Ex (x, p) => quant Ex (x, simp p)
          | f => f
      in
         fn f =>
            let
               val cs = conjuncts f
               val cs = List.setify eq cs
            in
               simp (listConj cs)
            end
      end

   val fillHolesWithTop =
      let
         val rec f = fn
            imogen.And (p, q) => imogen.And (f p, f q)
          | imogen.Imp (r, p) => imogen.Imp (r, f p)
          | All (x, p) => All (x, f p)
          | Ex (x, p) => Ex (x, f p)
          | Hole => Top
          | p => p
      in
         f
      end

   datatype t = datatype Export.t

   val () = noWarnUnused ()
end
