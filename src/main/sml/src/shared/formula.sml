
structure Formula :> Formula = struct
   structure U = Unicode

   open General
   open PP.Ops

   structure Export = struct
      datatype t =
         Atom of Rel.t
       | Top
       | Bot
       | Not of t
       | And of t * t
       | Or of t * t
       | Imp of t * t
       | Iff of t * t
       | All of (Var.t * Sort.t) * t
       | Ex of (Var.t * Sort.t) * t
       | Label of string * t
   end
   datatype t = datatype Export.t

   type printable = t
   type eqable = t

   val rec atoms = fn
      Atom rel => Rel.atoms rel
    | Label (_, p) => atoms p
    | Not p => atoms p
    | And (p,q) => Atoms.union (atoms p, atoms q)
    | Or (p,q) => Atoms.union (atoms p, atoms q)
    | Imp (p,q) => Atoms.union (atoms p, atoms q)
    | Iff (p,q) => Atoms.union (atoms p, atoms q)
    | All ((x, _), p) => Atoms.remove (atoms p, Left x)
    | Ex ((x, _), p) => Atoms.remove (atoms p, Left x)
    | Top => Atoms.empty
    | Bot => Atoms.empty

   (* Rename free occurrances of x to y *)
   val rename : Var.t * Var.t -> t -> t = fn
      (x, y) =>
      let
         val xy = (x, Term.Var y)
         fun rel r = Rel.apply1 (r, xy)
         fun f fm = case fm of
            Atom r => Atom (rel r)
          | Label (s, p) => Label (s, f p)
          | All ((x', s), a) =>
            if Var.eq (x, x') then fm else All ((x', s), f a)
          | Ex ((x', s), a) =>
            if Var.eq (x, x') then fm else Ex ((x', s), f a)
          | Not p => Not (f p)
          | And (p, q) => And (f p, f q)
          | Or (p, q) => Or (f p, f q)
          | Imp (p, q) => Imp (f p, f q)
          | Iff (p, q) => Iff (f p, f q)
          | _ => fm
      in
         f
      end

   fun unlabel f = case f of
      Label (_, p) => unlabel p
    | Not p => Not (unlabel p)
    | And (p, q) => And (unlabel p, unlabel q)
    | Or (p, q) => Or (unlabel p, unlabel q)
    | Imp (p, q) => Imp (unlabel p, unlabel q)
    | Iff (p, q) => Iff (unlabel p, unlabel q)
    | All (bnd, a) => All (bnd, unlabel a)
    | Ex (bnd, a) => Ex (bnd, unlabel a)
    | f => f

   fun apply1 (f, xt as (x, t)) =
      let
         fun subst fm = case fm of
            Atom rel => Atom (Rel.apply1 (rel, xt))
          | Label (s, p) => Label (s, subst p)
          | All ((x', s), a) =>
            if Var.eq (x, x')
            then fm
            else if Atoms.mem (Term.atoms t, Left x') then
               let
                  val v = Var.next ()
                  val a' = rename (v, x') a
               in
                  All ((v, s), subst a')
               end
            else All ((x', s), subst a)
          | Ex ((x', s), a) =>
            if Var.eq (x, x')
            then fm
            else if Atoms.mem (Term.atoms t, Left x') then
               let
                  val v = Var.next ()
                  val a' = rename (v, x') a
               in
                  Ex ((v, s), subst a')
               end
            else Ex ((x', s), subst a)
          | Not p => Not (subst p)
          | And (p, q) => And (subst p, subst q)
          | Or (p, q) => Or (subst p, subst q)
          | Imp (p, q) => Imp (subst p, subst q)
          | Iff (p, q) => Iff (subst p, subst q)
          | _ => fm
      in
         subst f
      end

   local
      fun veq ((x1, s1), (x2, s2)) =
         Var.eq (x1, x2) andalso Sort.Base.eq (s1, s2)
   in
      fun eq' f  ts =
         let
            val eq = eq' f
         in
            case ts of
               (Atom rel1, Atom rel2) => Rel.eq' f (rel1, rel2)
             | (Label (s1, p1), Label (s2, p2)) => true
             | (Top, Top) => true
             | (Bot, Bot) => true
             | (Not x, Not y) => eq (x, y)
             | (And (x1, y1), And (x2, y2)) => eq (x1, x2) andalso eq (y1, y2)
             | (Or (x1, y1), Or (x2, y2)) => eq (x1, x2) andalso eq (y1, y2)
             | (Imp (x1, y1), Imp (x2, y2)) => eq (x1, x2) andalso eq (y1, y2)
             | (Iff (x1, y1), Iff (x2, y2)) => eq (x1, x2) andalso eq (y1, y2)
             | (All (x, f), All (y, g)) => veq (x, y) andalso eq (f, g)
             | (Ex (x, f), Ex (y, g)) => veq (x, y) andalso eq (f, g)
             | _ => false
         end
      val eq = eq' Term.eq
   end

   (* ----------------------------------------------------------------------- *)
   (*  Printing                                                               *)
   (* ----------------------------------------------------------------------- *)

   local
      structure Prec = Parse.Prec

      val prec = fn
         Iff _ => Prec.Iff
       | Imp _ => Prec.Imp
       | Or _ => Prec.Or
       | And _ => Prec.And
       | Not _ => Prec.Not
       | All _ => Prec.Quant
       | Ex _ => Prec.Quant
       | Label _ => Prec.Label
       | _ => Prec.Atom
      val rec destAll = fn
         All (x, b) =>
         let
            val (xs, b') = destAll b
         in
            (x::xs, b')
         end
       | f => ([], f)
      val rec destEx = fn
         Ex (x, b) =>
         let
            val (xs, b') = destEx b
         in
            (x::xs, b')
         end
       | f => ([], f)
   in
      fun pp (f : printable) =
         let
            val fprec = prec f
            fun var (x, s) = %[Var.pp x, $":", Sort.Base.pp s]
         in
            case f of
               Atom rel => Rel.pp rel
             | Label (s, p) => %[$s, $"::", pp p]
             | Top => $U.top
             | Bot => $U.bot
             | Not p =>
               let
                  val p' = pp p
                  val p' = if prec p < fprec then PP.paren p' else p'
               in
                  %[$U.neg, p']
               end
             | And (p, q) =>
               let
                  val p' = pp p
                  val p' = if prec p < fprec then PP.paren p' else p'
                  val q' = pp q
                  val q' = if prec q <= fprec then PP.paren q' else q'
               in
                  PP.hang (%[p', \, $U.wedge]) 2 q'
               end
             | Or (p, q) =>
               let
                  val p' = pp p
                  val p' = if prec p < fprec then PP.paren p' else p'
                  val q' = pp q
                  val q' = if prec q <= fprec then PP.paren q' else q'
               in
                  PP.hang (%[p', \, $U.vee]) 2 q'
               end
             | Imp (p, q) =>
               let
                  val p' = pp p
                  val p' = if prec p <= fprec then PP.paren p' else p'
                  val q' = pp q
                  val q' = if prec q < fprec then PP.paren q' else q'
               in
                  PP.hang (%[p', \, $U.sup]) 2 q'
               end
             | Iff (p, q) =>
               let
                  val p' = pp p
                  val p' = if prec p <= fprec then PP.paren p' else p'
                  val q' = pp q
                  val q' = if prec q < fprec then PP.paren q' else q'
               in
                  PP.hang (%[p', \, $U.iff]) 2 q'
               end
             | All _ =>
               let
                  val (xs, p) = destAll f
                  val p' = pp p
                  val p' = if prec p < fprec then PP.paren p' else p'
               in
                  PP.hang (%[$U.all, \, %(PP.punctuate \ (map var xs)), $"."]) 2 p'
               end
             | Ex _ =>
               let
                  val (xs, p) = destEx f
                  val p' = pp p
                  val p' = if prec p < fprec then PP.paren p' else p'
               in
                  PP.hang (%[$U.exists, \, %(PP.punctuate \ (map var xs)), $"."]) 2 p'
               end
         end
   end (* local *)

   val () = noWarnUnused ()
end
