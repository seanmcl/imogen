
structure Formula :> Formula = struct
   structure F = PFormula
   structure T = Term
   structure S = Sort
   structure Prec = Parse.Prec
   structure P = Parse
   structure PF = Parse.Formula
   structure U = Unicode

   open General
   open PP.Ops

   datatype pos =
      PAtom of Term.t
    | Tensor of pos * pos
    | One
    | Sum of pos * pos
    | Zero
    | Down of neg
    | Bang of neg
    | Ex of Var.t * pos
   and neg =
      NAtom of Term.t
    | With of neg * neg
    | Top
    | Lolli of pos * neg
    | Up of pos
    | All of Var.t * neg
   type printable = neg
   type t = neg

   val eps = T.Fn (Func.Linear.eps, [])
   fun a * b = T.Fn (Func.Linear.times, [a, b])
   val ` = T.Var

   (* p ⓦ w *)
   fun relWorld (p, w) = Rel.make (Pred.Linear.atw, [p, w])

   (* n ⓕ f *)
   fun relFrame (n, (h, w)) = Rel.make (Pred.Linear.atf, [n, h, w])

   (*
        P           P @ k
      ----------------------------------------
        0           ⊤
        P1 ⊕ P2     P1 @ k ∧ P2 @ k
        1           k(ε)
        P1 ⊗ P2     P1 @ α. P2 @ β. k(α ⋆ β)
        a+          ∀ α. a+ @ α ⊃ k(α)
        ↓ N         ∀ α. ↓ (N @ α) ⊃ k(α)
        ! N         ↓ (N @ ε) ⊃ k(ε)
        ∃ x. N      ∀ x. P @ k
   *)
   fun atCont (f, k) = case f of
      Zero => F.Top
    | Sum (p1, p2) =>
      let
         val p1 = atCont (p1, k)
         val p2 = atCont (p2, k)
      in
         F.With (p1, p2)
      end
    | One => k eps
    | Tensor (p1, p2) =>
      atCont (p1, fn a => atCont (p2, fn b => k (a * b)))
    | PAtom a =>
      let
         val w = Var.next ()
         val f = k (`w)
      in
         F.All ((w, S.LWorld), F.Lolli (F.PAtom (relWorld (a, `w)), f))
      end
    | Down n =>
      let
         val w = Var.next ()
         val n = atWorld (n, T.Var w)
         val f = k (T.Var w)
      in
         F.All ((w, S.LWorld), F.Lolli (F.Down n, f))
      end
    | Bang n =>
      let
         val n = atWorld (n, eps)
         val p = k eps
      in
         F.Lolli (F.Down n, p)
      end
    | Ex (x, p) => F.All ((x, S.I), atCont (p, k))

   (*
      Note that for the negative translation we need to
      modify the ∀ frame variable quantifier since we don't
      allow frame variables.  For instance, in the paper the rule
      for negative atoms is

        a-    ↦    ∀ φ. a- @ φ ⊃ φ ◃ ρ

      We'll translate this as

        a-    ↦    ∀ φ α. a- @ (φ ⊛ α) ⊃ (φ ⊛ α) ◃ ρ

      where φ is now a Head variable and α is a World variable.

        N           N @ ρ
      ---------------------------------------------
        ⊤          ⊤
        N1 ∧ N2    N1 @ ρ ∧ N2 @ ρ
        P ⊸ N      P @ α. N @ (p ⋆ α)
        a-         ∀ φ α. a- @ φ ⊛ α ⊃ φ ⊛ α ⋆ ρ
        ↑P         ∀ φ α. ↓(P @ α'. φ ⊛ α ⋆ α') ⊃ h ⊛ α ⋆ ρ
        ∀ x. N     ∀ x. N @ p
      *)
   and atWorld (f, w) = case f of
      Top => F.Top
    | With (n1, n2) =>
      let
         val n1 = atWorld (n1, w)
         val n2 = atWorld (n2, w)
      in
         F.With (n1, n2)
      end
    | Lolli (p, n) => atCont (p, fn w' => atWorld (n, w * w'))
    | NAtom a =>
      let
         val h = Var.next ()
         val w' = Var.next ()
         val f = (T.Var h, T.Var w')
      in
         F.All ((h, S.LHead),
                F.All ((w', S.LWorld),
                       F.Lolli (F.PAtom (relFrame (a, f)),
                                F.NAtom (Frame.make (Frame.extend (f, w))))))
      end
    | Up p =>
      let
         val h = Var.next ()
         val a = Var.next ()
         val f = (T.Var h, T.Var a)
         val p = atCont (p, fn a' => F.NAtom (Frame.make (Frame.extend (f, a'))))
      in
         F.All
            ((h, S.LHead),
             F.All ((a, S.LWorld),
                    F.Lolli (F.Down p,
                             F.NAtom (Frame.make (Frame.extend (f, w))))))
      end
    | All (x, p) => F.All ((x, S.I), atWorld (p, w))

   fun pformula f = atWorld (f, eps)

   val parse =
      let
         fun mkRel (P.Rel.R (p, ts)) =
            let
               val ts = map T.parse ts
               val f = P.Pred.toString p
               val f = if Char.isUpper (String.sub (f, 0)) then "_" ^ f else f
               val f = Func.ofString f
               val rel = T.Fn (f, ts)
            in
               case p of
                  P.Pred.NegId _ => Right rel
                | _ => Left rel
            end
         fun fail () = failwith "Parse error"
         val rec pos = fn
            PF.Rel rel =>
            let in
               case mkRel rel of
                  Left rel => PAtom rel
                | Right rel => Down (NAtom rel)
            end
          | PF.Const P.Const.One => One
          | PF.Const P.Const.Zero => Zero
          | PF.Unop (P.Unop.Down, n) => Down (neg n)
          | PF.Unop (P.Unop.Bang, n) => Bang (neg n)
          | PF.Binop (P.Binop.And, p, q) => Tensor (pos p, pos q)
          | PF.Binop (P.Binop.Tensor, p, q) => Tensor (pos p, pos q)
          | PF.Binop (P.Binop.Or, p, q) => Sum (pos p, pos q)
          | PF.Quant (P.Quant.Ex, (x, _), p) =>
            Ex (Var.ofString x, pos p)
          | f => Down (neg f)
         and neg = fn
            PF.Rel rel =>
            let in
               case mkRel rel of
                  Left rel => Up (PAtom rel)
                | Right rel => NAtom rel
            end
          | PF.Const P.Const.True => Top
          | PF.Const P.Const.Top => Top
          | PF.Unop (P.Unop.Bang, _) => fail ()
          | PF.Unop (P.Unop.Up, p) => Up (pos p)
          | PF.Unop (P.Unop.Not, n) => Lolli (pos n, Up Zero)
          | PF.Binop (P.Binop.With, p, q) => With (neg p, neg q)
          | PF.Binop (P.Binop.Imp, p, q) => Lolli (Bang (neg p), neg q)
          | PF.Binop (P.Binop.Lolli, p, q) => Lolli (pos p, neg q)
          | PF.Binop (P.Binop.Imp', p, q) => Lolli (Bang (neg q), neg p)
          | PF.Binop (P.Binop.Lolli', p, q) => Lolli (pos q, neg p)
          | PF.Binop (P.Binop.BiLolli, p, q) =>
            let
               val p = neg p
               val q = neg q
            in
               With (Lolli (Down p, q), Lolli (Down q, p))
            end
          | PF.Quant (P.Quant.All, (x, _), p) =>
            All (Var.ofString x, neg p)
          | f => Up (pos f)
      in
         neg
      end

   val pp =
      let
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
         val precP = fn
            Tensor _ => Prec.And
          | Sum _ => Prec.Or
          | Down _ => Prec.Not
          | Ex _ => Prec.Quant
          | _ => Prec.Atom
         val precN = fn
            With _ => Prec.And
          | Lolli _ => Prec.Imp
          | Up _ => Prec.Not
          | All _ => Prec.Quant
          | _ => Prec.Atom
         val rec pos = fn
            PAtom rel => Term.pp rel
          | One => $"1"
          | Zero => $"0"
          | Tensor (p, q) =>
            let
               val p' = pos p
               val p' = if precP p <= Prec.And then PP.paren p' else p'
               val q' = pos q
               val q' = if precP q < Prec.And then PP.paren q' else q'
            in
               PP.hang (%[p', \, $U.wedge]) 0 q'
            end
          | Sum (p, q) =>
            let
               val p' = pos p
               val p' = if precP p <= Prec.Or then PP.paren p' else p'
               val q' = pos q
               val q' = if precP q < Prec.Or then PP.paren q' else q'
            in
               PP.hang (%[p', \, $U.oplus]) 0 q'
            end
          | Down n =>
            let
               val n' = neg n
               val n' = if precN n < Prec.Not then PP.paren n' else n'
            in
               %[$U.down, n']
            end
          | Bang n =>
            let
               val n' = neg n
               val n' = if precN n < Prec.Not then PP.paren n' else n'
            in
               %[$"!", n']
            end
          | f as Ex _ =>
            let
               val (xs, p) = destEx f
               val p' = pos p
               val p' = if precP p < Prec.Quant then PP.paren p' else p'
            in
               PP.hang (%[$U.exists, \, %(PP.punctuate \ (List.map Var.pp xs)), $"."])
                  0 p'
            end
         and neg = fn
            NAtom rel => Term.pp rel
          | With (n, m) =>
            let
               val n' = neg n
               val n' = if precN n <= Prec.And then PP.paren n' else n'
               val m' = neg m
               val m' = if precN m < Prec.And then PP.paren m' else m'
            in
               PP.hang (%[n', \, $"&"]) 0 m'
            end
          | Top => $U.top
          | Lolli (p, n) =>
            let
               val p' = pos p
               val p' = if precP p <= Prec.Imp then PP.paren p' else p'
               val n' = neg n
               val n' = if precN n < Prec.Imp then PP.paren n' else n'
            in
               PP.hang (%[ p', \, $"-o"]) 0 n'
            end
          | Up p =>
            let
               val n' = pos p
               val n' = if precP p < Prec.Not then PP.paren n' else n'
            in
               %[$U.up, n']
            end
          | f as All _ =>
            let
               val (xs, p) = destAll f
               val p' = neg p
               val p' = if precN p < Prec.Quant then PP.paren p' else p'
            in
               PP.hang (%[$U.all, \, %(PP.punctuate \ (List.map Var.pp xs)), $"."]) 0 p'
            end
      in
         neg
      end

   val usesConstraints = true
   val () = noWarnUnused (fn _ : printable * t => ())
end
