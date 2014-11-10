
structure Simpson :> Simpson = struct
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
    | Dia of pos
    | Ex of Var.t * pos
    | Down of neg
   and neg =
      NAtom of Term.t
    | Top
    | With of neg * neg
    | imogen.Imp of pos * neg
    | Iff of neg * neg
    | Box of neg
    | All of Var.t * neg
    | Up of pos
   type t = neg
   type printable = t
   type parseable = t

   val pp =
      let
         val precP = fn
            imogen.And _ => Prec.imogen.And
          | Or _ => Prec.Or
          | Down _ => Prec.Not
          | Dia _ => Prec.Not
          | Ex _ => Prec.Quant
          | _ => Prec.imogen.Atom
         val precN = fn
            With _ => Prec.imogen.And
          | imogen.Imp _ => Prec.imogen.Imp
          | Up _ => Prec.Not
          | Box _ => Prec.Not
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
          | One => $"1"
          | Zero => $"0"
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
          | Dia p =>
            let
               val p' = pos p
               val p' = if precP p < Prec.Not then PP.paren p' else p'
            in
               %[$U.dia, p']
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
          | Box p =>
            let
               val n' = neg p
               val n' = if precN p < Prec.Not then PP.paren n' else n'
            in
               %[$U.box, n']
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
               (* val f = Func.ofString (P.Pred.toString p) *)
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
          | PF.Unop (P.Unop.Dia, n) => Dia (pos n)
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
          | PF.Unop (P.Unop.Not, n) => imogen.Imp (pos n, Up Zero)
          | PF.Unop (P.Unop.Box, n) => Box (neg n)
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

   val pformula =
      let
         fun mkRel w t = Rel.make (Pred.Modal.atw, [t, w])
         fun mkLe w w' =
            F.PAtom (Rel.make (Pred.Modal.le, [w, w']))
         fun pos w = fn
            PAtom r => F.PAtom (mkRel w r)
          | One => F.One
          | Zero => F.Zero
          | imogen.And (p, q) => F.Tensor (pos w p, pos w q)
          | Or (p, q) => F.Sum (pos w p, pos w q)
          | Dia p =>
            let
               val w' = Var.next ()
               val wt : Term.t = Term.Var w'
            in
               F.Ex ((w', S.MWorld), F.Tensor (mkLe w wt, pos wt p))
            end
          | Ex (x, p) => F.Ex ((x, S.I), pos w p)
          | Down f => F.Down (neg w f)
         and neg w = fn
            NAtom r : neg => F.NAtom (mkRel w r) : F.neg
          | Top => F.Top
          | With (p, q) => F.With (neg w p, neg w q)
          | imogen.Imp (p, q) => F.Lolli (pos w p, neg w q)
          | Iff (p, q) => F.BiLolli (neg w p, neg w q)
          | Box p =>
            let
               val w' = Var.next ()
               val wt = Term.Var w'
            in
               F.All ((w', S.MWorld), F.Lolli (mkLe w wt, neg wt p))
            end
          | All (x, p) => F.All ((x, S.I), neg w p)
          | Up f => F.Up (pos w f)
      in
         fn f => neg (Term.Fn (Func.Modal.init, [])) f
      end

   val usesConstraints = true
   val () = noWarnUnused (fn _ : parseable => (ofString))
end
