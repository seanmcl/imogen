
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
    | Dot of pos * pos
    | One
    | Sum of pos * pos
    | Zero
    | Down of neg
    | Bang of neg
    | UBang of neg
    | Ex of Var.t * pos
   and neg =
      NAtom of Term.t
    | With of neg * neg
    | Top
    | ImpL of pos * neg (* >-> *)
    | ImpR of pos * neg (* ->> *)
    | Up of pos
    | All of Var.t * neg
   type printable = neg
   type t = neg

   val ` = Term.Var
   val eps = Term.Fn (Func.Ordered.eps, [])
   fun a * b = Term.Fn (Func.Ordered.times, [a, b])

   (* p ⓦ w *)
   fun relWorld (p, w) =
      Rel.make (Pred.Ordered.atw, [p, w])

   (* n ⓕ f *)
   fun relFrame (n, (w1, h, w2)) =
      Rel.make (Pred.Ordered.atf, [n, w1, h, w2])

   (*
     P           P @ k
   ----------------------------------------
     0           ⊤
     P1 ⊕ P2     P1 @ k ∧ P2 @ k
     1           k(ε)
     P1 ● P2     P1 @ α. P2 @ β. k(α · β)
     a+          ∀ α. a+ @ α ⊃ k(α)
     ↓ N         ∀ α. ↓ (N @ α) ⊃ k(α)
     ! N         ↓ (N @ ε) ⊃ k(ε)
     ¡ N         ∀ α'. ↓ (N @ i α') ⊃ k (i α')
     ∃ x. N      ∀ x. P @ k
      *)

   fun atCont (f, k : Term.t -> F.neg) = case f of
      Zero => F.Top
    | Sum (p1, p2) => F.With (atCont (p1, k), atCont (p2, k))
    | One => k eps
    | Dot (p1, p2) =>
      atCont (p2, fn a => atCont (p1, fn b => k (a * b)))
    | PAtom a =>
      let
         val w = Var.next ()
         val f = k (`w)
      in
         F.All ((w, S.OWorld), F.Lolli (F.PAtom (relWorld (a, `w)), f))
      end
    | Down n =>
      let
         val w = Var.next ()
         val n = atWorld (n, `w)
         val f = k (`w)
      in
         F.All ((w, S.OWorld), F.Lolli (F.Down n, f))
      end
    | UBang n =>
      let
         val w = Var.next ()
         val w' = World.ofLinear (T.Var w)
      in
         F.All ((w, S.LWorld), (* NB. LWorld is not a typo. *)
                F.Lolli (F.Down (atWorld (n, w')), k w'))
      end
    | Bang n => F.Lolli (F.Down (atWorld (n, eps)), k eps)
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
        P >-> N    P @ α. N @ (α ⋆ p)
        P ->> N    P @ α. N @ (p ⋆ α)
        P ⊸ N      P @ α. N @ (p ⋆ α)
        a-         ∀ φ α. a- @ (φ ⊛ α) ⊃ (φ ⊛ α) ◃ ρ
        ↑P         ∀ h α. ↓(P @ α'. h ⊛ α ⋆ α') ⊃ h ⊛ α ⋆ ρ
        ∀ x. N     ∀ x. N @ p
      *)

   and atWorld (f, w) = case f of
      Top => F.Top
    | With (n1, n2) => F.With (atWorld (n1, w), atWorld (n2, w))
    | ImpL (p, n) => atCont (p, fn a => atWorld (n, a * w))
    | ImpR (p, n) => atCont (p, fn a => atWorld (n, w * a))
    | NAtom a =>
      let
         val l = Var.next ()
         val h = Var.next ()
         val r = Var.next ()
         val f = (`l, `h, `r)
      in
         F.All ((l, S.OWorld),
                F.All ((h, S.OHead),
                       F.All ((r, S.OWorld),
                              F.Lolli (F.PAtom (relFrame (a, f)),
                                       F.NAtom (Frame.make (Frame.extend (f, w)))))))
      end
    | Up p =>
      let
         val l = Var.next ()
         val h = Var.next ()
         val r = Var.next ()
         val f = (`l, `h, `r)
         val p = atCont (p, fn w => F.NAtom (Frame.make (Frame.extend (f, w))))
      in
         F.All ((l, S.OWorld),
                F.All ((h, S.OHead),
                       F.All ((r, S.OWorld),
                              F.Lolli (F.Down p, F.NAtom (Frame.make (Frame.extend (f, w)))))))
      end
    | All (x, p) => F.All ((x, S.I), atWorld (p, w))

   fun pformula f = atWorld (f, eps)

   val parse =
      let
         fun mkRel (P.Rel.R (p, ts)) =
            let
               val ts = map T.parse ts
               val f = Func.ofString (P.Pred.toString p)
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
          | PF.Unop (P.Unop.UBang, n) => UBang (neg n)
          | PF.Binop (P.Binop.And, p, q) => Dot (pos p, pos q)
          | PF.Binop (P.Binop.Tensor, p, q) => Dot (pos p, pos q)
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
          | PF.Binop (P.Binop.With, p, q) => With (neg p, neg q)
          | PF.Binop (P.Binop.OrdImp1, p, q) => ImpL (pos p, neg q)
          | PF.Binop (P.Binop.OrdImp2, p, q) => ImpR (pos q, neg p)
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
            Dot _ => Prec.And
          | Sum _ => Prec.Or
          | Down _ => Prec.Not
          | Ex _ => Prec.Quant
          | _ => Prec.Atom
         val precN = fn
            With _ => Prec.And
          | ImpL _ => Prec.Imp
          | ImpR _ => Prec.Imp
          | Up _ => Prec.Not
          | All _ => Prec.Quant
          | _ => Prec.Atom
         val rec pos = fn
            PAtom rel => Term.pp rel
          | One => $"1"
          | Zero => $"0"
          | Dot (p, q) =>
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
          | UBang n =>
            let
               val n' = neg n
               val n' = if precN n < Prec.Not then PP.paren n' else n'
            in
               %[$U.uexcl, n']
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
          | ImpL (p, n) =>
            let
               val p' = pos p
               val p' = if precP p <= Prec.Imp then PP.paren p' else p'
               val n' = neg n
               val n' = if precN n < Prec.Imp then PP.paren n' else n'
            in
               PP.hang (%[ p', \, $">->"]) 0 n'
            end
          | ImpR (p, n) =>
            let
               val p' = pos p
               val p' = if precP p <= Prec.Imp then PP.paren p' else p'
               val n' = neg n
               val n' = if precN n < Prec.Imp then PP.paren n' else n'
            in
               PP.hang (%[ p', \, $"->>"]) 0 n'
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
