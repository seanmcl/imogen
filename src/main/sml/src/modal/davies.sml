
structure Davies :> Davies = struct
   structure U = Unicode
   structure Prec = Parse.Prec
   structure S = Sort
   structure F = PFormula
   structure P = Parse
   structure PF = Parse.imogen.Formula
   structure T = Term

   open General
   open PP.Ops

   datatype pos =
      PAtom of Term.t
    | One
    | Zero
    | imogen.And of pos * pos
    | Or of pos * pos
    | Box of neg
    | Ex of Var.t * pos
    | Down of neg
   and neg =
      NAtom of Term.t
    | Top
    | With of neg * neg
    | imogen.Imp of pos * neg
    | Not of pos
    | Iff of neg * neg
    | Dia of pos
    | All of Var.t * neg
    | Up of pos
   type t = neg
   type printable = t
   type parseable = t
   val ` = T.Var

   val pformula =
      let
         fun mkLe (w, w') =
            F.PAtom (Rel.make (Pred.Modal.le, [w, w']))
         fun here (w, f) =
            F.NAtom (Rel.make (Pred.Modal.here, [w, f]))
         fun poss (f) =
            F.PAtom (Rel.make (Pred.Modal.poss, [f]))
         (*
            n         n @ w
            -----------------------------
            p ⊃ n     p @ w ⊃ n @ w
            n1 & n2   n1 @ w & n2 @ w
            T         T
            ◇ p       ∀ φ. poss(φ) ⊃ ↓ (∀ v ≥ w. p @ w ⊃ h(v, φ)) ⊃ h(w, φ)
            a         ∀ φ.  ⓕ(a, w, φ) ⊃ h(w, φ)
            ↑p        ∀ φ. ↓ (p @ w ⊃ h(w, φ)) ⊃ h(w, φ)
          *)
         fun neg w = fn
            NAtom t =>
            let
               val f = Var.next ()
               val r = Rel.make (Pred.Modal.natom, [t, w, `f])
            in
               F.All ((f, S.MWorld),
                      F.Lolli (F.PAtom r, here (w, `f)))
            end
          | With (a, b) => F.With (neg w a, neg w b)
          | Top => F.Top
          | imogen.Imp (a, b) => F.Lolli (pos w a, neg w b)
          | Not a => neg w (imogen.Imp (a, Up Zero))
          | Iff (a, b) => neg w (With (imogen.Imp (Down a, b), imogen.Imp (Down b, a)))
          | Dia a =>
            let
               val f = Var.next ()
               val v = Var.next ()
               val p = pos (`v) a
               val res =
                   F.All ((f, S.MWorld),
                          F.Lolli
                              (poss (`f),
                               F.Lolli
                                   (F.Down (F.All ((v, S.MWorld),
                                            F.Lolli (F.Tensor (mkLe (w, `v), p),
                                                     here (`v, `f)))),
                                    here (w, `f) )))
            in
               res
            end
          | Up a =>
            let
               val f = Var.next ()
               val p = pos w a
            in
               F.All ((f, S.MWorld),
                      F.Lolli ( F.Down (F.Lolli (p, here (w, `f))),
                                here (w, `f)))
            end
          | All (x, a) => F.All ((x, S.I), neg w a)

         (*
           p         p @ w
           --------------------------
           p1 ∧ p2   p1 @ w ∧ p2 @ w
           p1 ∨ p2   p1 @ w ∨ p2 @ w
           1         1
           0         0
           □ n       ↓ ∀ v ≥ w. n @ v
           a         a ⓦ w
           ↓ n       ↓ (n @ w)
         *)
         and pos w = fn
            PAtom t => F.PAtom (Rel.make (Pred.Modal.patom, [t, w]))
          | One => F.One
          | Zero => F.Zero
          | imogen.And (p, q) => F.Tensor (pos w p, pos w q)
          | Or (p, q) => F.Sum (pos w p, pos w q)
          | Box p =>
            let
               val w' = Var.next ()
               val wt = Term.Var w'
            in
               F.Down (F.All ((w', S.MWorld), F.Lolli (mkLe (w, wt), neg wt p)))
            end
          | Ex (x, p) => F.Ex ((x, S.I), pos w p)
          | Down f => F.Down (neg w f)
      in
         fn f =>
            let
               val w = (Term.Fn (Func.Modal.init, []))
               val res = neg w f
            in
               res
            end
      end

   val pp =
      let
         val precP = fn
            imogen.And _ => Prec.imogen.And
          | Or _ => Prec.Or
          | Down _ => Prec.Not
          | Box _ => Prec.Not
          | Ex _ => Prec.Quant
          | _ => Prec.imogen.Atom
         val precN = fn
            With _ => Prec.imogen.And
          | imogen.Imp _ => Prec.imogen.Imp
          | Up _ => Prec.Not
          | Dia _ => Prec.Not
          | All _ => Prec.Quant
          | _ => Prec.imogen.Atom
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
         val rec pos = fn
            PAtom r => Term.pp r
          | One => $U.top
          | Zero => $U.bot
          | imogen.And (p, q) =>
            let
               val p' = pos p
               val p' = if precP p <= Prec.imogen.And then PP.paren p' else p'
               val q' = pos q
               val q' = if precP q < Prec.imogen.And then PP.paren q' else q'
            in
               PP.hang (%[p', \, $U.wedge]) 0 q'
            end
          | Or (p, q) =>
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
          | Box p =>
            let
               val p' = neg p
               val p' = if precN p < Prec.Not then PP.paren p' else p'
            in
               %[$U.box, p']
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
            NAtom t => Term.pp t
          | With (n, m) =>
            let
               val n' = neg n
               val n' = if precN n <= Prec.imogen.And then PP.paren n' else n'
               val m' = neg m
               val m' = if precN m < Prec.imogen.And then PP.paren m' else m'
            in
               PP.hang (%[n', \, $"&"]) 0 m'
            end
          | Top => $U.top
          | imogen.Imp (p, n) =>
            let
               val p' = pos p
               val p' = if precP p <= Prec.imogen.Imp then PP.paren p' else p'
               val n' = neg n
               val n' = if precN n < Prec.imogen.Imp then PP.paren n' else n'
            in
               PP.hang (%[ p', \, $U.sup]) 0 n'
            end
          | Not n =>
            let
               val n' = pos n
               val n' = if precP n < Prec.Not then PP.paren n' else n'
            in
               %[$U.neg, n']
            end
          | Iff (a, b) =>
            let
               val a' = neg a
               val a' = if precN a <= Prec.imogen.Imp then PP.paren a' else a'
               val b' = neg b
               val b' = if precN b < Prec.imogen.Imp then PP.paren b' else b'
            in
               PP.hang (%[ a', \, $U.sup]) 0 b'
            end
          | Up p =>
            let
               val n' = pos p
               val n' = if precP p < Prec.Not then PP.paren n' else n'
            in
               %[$U.up, n']
            end
          | Dia p =>
            let
               val n' = pos p
               val n' = if precP p < Prec.Not then PP.paren n' else n'
            in
               %[$U.dia, n']
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

   val parse =
      let
         fun mkRel (P.Rel.R (p, ts)) =
            let
               val ts = map T.parse ts
               val f = Pred.toFunc (Pred.parse p)
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
          | PF.Const P.Const.Top => One
          | PF.Const P.Const.True => One
          | PF.Const P.Const.One => One
          | PF.Const P.Const.False => Zero
          | PF.Const P.Const.Zero => Zero
          | PF.Unop (P.Unop.Down, n) => Down (neg n)
          | PF.Unop (P.Unop.Box, n) => Box (neg n)
          | PF.Binop (P.Binop.imogen.And, p, q) => imogen.And (pos p, pos q)
          | PF.Binop (P.Binop.Tensor, p, q) => imogen.And (pos p, pos q)
          | PF.Binop (P.Binop.Or, p, q) => Or (pos p, pos q)
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
          | PF.Unop (P.Unop.Not, n) => Not (pos n)
          | PF.Unop (P.Unop.Dia, p) => Dia (pos p)
          | PF.Binop (P.Binop.With, p, q) => With (neg p, neg q)
          | PF.Binop (P.Binop.imogen.Imp, p, q) => imogen.Imp (pos p, neg q)
          | PF.Binop (P.Binop.imogen.Imp', p, q) => imogen.Imp (pos q, neg p)
          | PF.Binop (P.Binop.Iff, p, q) =>
            let
               val p = neg p
               val q = neg q
            in
               With (imogen.Imp (Down p, q), imogen.Imp (Down q, p))
            end
          | PF.Quant (P.Quant.All, (x, _), p) =>
            All (Var.ofString x, neg p)
          | f => Up (pos f)
      in
         neg
      end

   val ofString = parse o PF.ofString
   val usesConstraints = true

   val () = noWarnUnused (fn _ : printable * parseable => (ofString))
end
