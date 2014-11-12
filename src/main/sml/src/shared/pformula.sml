
structure PFormula :> PFormula = struct
   structure F = Formula
   structure P = Parse
   structure Prec = Parse.Prec
   structure PF = Parse.Formula
   structure U = Unicode
   structure RSet = Pred.Set
   structure S = Signat
   structure T = Term

   open General
   open PP.Ops

   structure Export = struct
      datatype pos =
         PAtom of Rel.t
       | Tensor of pos * pos
       | One
       | Sum of pos * pos
       | Zero
       | Ex of (Var.t * Sort.t) * pos
       | Down of neg
       | PLabel of string * pos

      and neg =
         NAtom of Rel.t
       | With of neg * neg
       | Top
       | Lolli of pos * neg
       | BiLolli of neg * neg
       | All of (Var.t * Sort.t) * neg
       | Up of pos
       | NLabel of string * neg
   end
   datatype pos = datatype Export.pos
   datatype neg = datatype Export.neg

   structure PN = struct
      type ('a, 'b) t =
         { p : pos -> 'a
         , n : neg -> 'b }
      type 'a func = ('a, 'a) t
      type transform = (pos, neg) t
      fun pos {p, n=_} f = p f
      fun neg {p=_, n} f = n f
   end
   open PN

   val size =
      let
         val rec pos = fn
            Tensor (a, b) => 1 + pos a + pos b
          | Sum (a, b) => 1 + pos a + pos b
          | Down n => 1 + neg n
          | _ => 1
         and neg = fn
            With (a, b) => 1 + neg a + neg b
          | Lolli (a, b) => 1 + pos a + neg b
          | BiLolli (a, b) => 1 + neg a + neg b
          | Up p => 1 + pos p
          | _ => 1
      in
         { p = pos, n = neg }
      end

   fun map f =
      let
         val rec pos = fn
            PAtom r => PAtom (f r)
          | PLabel (s, p) => PLabel (s, pos p)
          | Tensor (p, q) => Tensor (pos p, pos q)
          | One => One
          | Sum (p, q) => Sum (pos p, pos q)
          | Zero => Zero
          | Ex (x, p) => Ex (x, pos p)
          | Down p => Down (neg p)
         and neg = fn
            NAtom r => NAtom (f r)
          | NLabel (s, p) => NLabel (s, neg p)
          | With (p, q) => With (neg p, neg q)
          | Top => Top
          | Lolli (p, q) => Lolli (pos p, neg q)
          | BiLolli (p, q) => BiLolli (neg p, neg q)
          | All (x, p) => All (x, neg p)
          | Up p => Up (pos p)
      in
         { p = pos, n = neg }
      end

   val shift2 =
      {p = Down o Up, n = Up o Down}

   fun fold f =
      let
         fun pos x = fn
            PAtom r => f (r, x)
          | PLabel (_, p) => pos x p
          | Tensor (p, q) => pos (pos x p) q
          | One => x
          | Sum (p, q) => pos (pos x p) q
          | Zero => x
          | Ex (_, p) => pos x p
          | Down p => neg x p
         and neg x = fn
            NAtom r => f (r, x)
          | NLabel (_, p) => neg x p
          | With (p, q) => neg (neg x p) q
          | Top => x
          | Lolli (p, q) => neg (pos x p) q
          | BiLolli (p, q) => neg (neg x p) q
          | All (_, p) => neg x p
          | Up p => pos x p
      in
         {p = pos, n = neg}
      end

   val rec mkLolli = fn
      ([], f) => f
    | (h::hs, f) => Lolli (h, mkLolli (hs, f))

   val destLolli =
      let
         fun f acc = fn
            Lolli (p, n) => f (p::acc) n
          | f => (rev acc, f)
      in
         f []
      end

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

   (* fun listQuant q (xs, b) = foldr q b xs *)

   (* val listAll = listQuant All *)

   val preds =
      let
         datatype occ = POS | NEG
         (* occurances *)
         fun opp POS = NEG
           | opp NEG = POS
         fun init POS b = (RSet.singleton (Rel.pred b), RSet.empty)
           | init NEG b = (RSet.empty, RSet.singleton (Rel.pred b))
         fun join ((p1, n1), (p2, n2)) = (RSet.union (p1, p2), RSet.union (n1, n2))
         val empty = (RSet.empty, RSet.empty)
         fun pos occ = fn
            PAtom p' => init occ p'
          | PLabel (_, p) => pos occ p
          | Tensor (a, b) => join (pos occ a, pos occ b)
          | One => empty
          | Sum (a, b) => join (pos occ a, pos occ b)
          | Zero => empty
          | Down a => neg occ a
          | Ex (_, a) => pos occ a
         and neg occ = fn
            NAtom p => init occ p
          | NLabel (_, p) => neg occ p
          | With (a,b) => join (neg occ a, neg occ b)
          | Top => empty
          | Lolli (a, b) => join (pos (opp occ) a, neg occ b)
          | BiLolli (a, b) =>
            let
               val (p1, n1) = neg occ a
               val (p2, n2) = neg occ b
               val all = foldr RSet.union RSet.empty [p1, n1, p2, n2]
            in
               (all, all)
            end
          | Up p => pos occ p
          | All (_, a) => neg occ a
         fun atomsP f =
            let
               val (pos, neg) = pos POS f
            in
               {pos=pos, neg=neg}
            end
         fun atomsN f =
            let
               val (pos, neg) = neg POS f
            in
               {pos=pos, neg=neg}
            end
      in
         {p = atomsP, n = atomsN}
      end

   val rec stripAll = fn
      All (x, p) =>
      let
         val (xs, p') = stripAll p
      in
         (x::xs, p')
      end
    | f => ([], f)

   val rec stripEx = fn
      Ex (x, p) =>
      let
         val (xs, p') = stripEx p
      in
         (x :: xs, p')
      end
    | f => ([], f)

   val pp =
      let
         val precP = fn
            Tensor _ => Prec.And
          | Sum _ => Prec.Or
          | Down _ => Prec.Not
          | Ex _ => Prec.Quant
          | PLabel _ => Prec.Label
          | _ => Prec.Atom
         val precN = fn
            With _ => Prec.And
          | Lolli _ => Prec.Imp
          | Up _ => Prec.Not
          | BiLolli _ => Prec.Iff
          | All _ => Prec.Quant
          | NLabel _ => Prec.Label
          | _ => Prec.Atom
         fun vs (x, s) =
            %[Var.pp x, case s of Sort.I => PP.empty | _ => %[$":", Sort.Base.pp s]]
         val rec pos = fn
            PAtom rel => Rel.pp rel
          | One => $U.top
          | PLabel (s, p) => %[$s, $"::", pos p]
          | Zero => $U.bot
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
          | f as Ex _ =>
            let
               val (xs, p) = destEx f
               val p' = pos p
               val p' = if precP p < Prec.Quant then PP.paren p' else p'
            in
               PP.hang (%[$U.exists, \, %(PP.punctuate \ (List.map vs xs)), $"."])
                  0 p'
            end
         and neg = fn
            NAtom rel => Rel.pp rel
          | NLabel (s, p) => %[$s, $"::", neg p]
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
               PP.hang (%[ p', \, $Unicode.sup]) 0 n'
            end
          | BiLolli (p, n) =>
            let
               (* Always print parens on bi-implications.  It's too confusing
                * to remember the associativity. *)
               val p' = neg p
               val p' = if precN p <= Prec.Iff then PP.paren p' else p'
               val n' = neg n
               val n' = if precN n <= Prec.Iff then PP.paren n' else n'
            in
               PP.hang (%[p', \, $Unicode.iff]) 0 n'
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
               PP.hang (%[$U.all, \, %(PP.punctuate \ (List.map vs xs)), $"."]) 0 p'
            end
      in
         {p = pos, n = neg}
      end

   val atoms =
      let
         val rec pos = fn
            PAtom rel => Rel.atoms rel
          | Tensor (p, q) => Atoms.union (pos p, pos q)
          | Sum (p, q) => Atoms.union (pos p, pos q)
          | Down n => neg n
          | Ex ((x, _), p) =>
            let
               val ats = pos p
            in
               Atoms.remove (ats, Left x)
            end
          | One => Atoms.empty
          | Zero => Atoms.empty
          | PLabel (_, p) => pos p
         and neg = fn
            NAtom rel => Rel.atoms rel
          | With (p, q) => Atoms.union (neg p, neg q)
          | Lolli (p, q) => Atoms.union (pos p, neg q)
          | BiLolli (p, q) => Atoms.union (neg p, neg q)
          | Up p => pos p
          | All ((x, _), p) =>
            let
               val ats = neg p
            in
               Atoms.remove (ats, Left x)
            end
          | Top => Atoms.empty
          | NLabel (_, p) => neg p
      in
         {p = pos, n = neg}
      end

   fun closed f = Atoms.isEmpty (neg atoms f)

   (* val ctx = *)
   (*    let *)
   (*       val rec pos = fn *)
   (*          PAtom rel => Ctx.infer (Right rel) *)
   (*        | Tensor (p, q) => Ctx.union (pos p, pos q) *)
   (*        | Sum (p, q) => Ctx.union (pos p, pos q) *)
   (*        | Down n => neg n *)
   (*        | Ex ((x, s), p) => *)
   (*          let *)
   (*             val ctx = pos p *)
   (*          in *)
   (*             case Ctx.find (ctx, Left x) of *)
   (*                NONE => ctx *)
   (*              | SOME s' => *)
   (*                if Sort.eq (s, s') then Ctx.remove (ctx, Left x) *)
   (*                else failwith "sort mismatch" *)
   (*          end *)
   (*        | One => Ctx.empty *)
   (*        | Zero => Ctx.empty *)
   (*       and neg = fn *)
   (*          NAtom rel => Ctx.infer (Right rel) *)
   (*        | With (p, q) => Ctx.union (neg p, neg q) *)
   (*        | Lolli (p, q) => Ctx.union (pos p, neg q) *)
   (*        | BiLolli (p, q) => Ctx.union (neg p, neg q) *)
   (*        | Up p => pos p *)
   (*        | All ((x, s), p) => *)
   (*          let *)
   (*             val ctx = neg p *)
   (*          in *)
   (*             case Ctx.find (ctx, Left x) of *)
   (*                NONE => ctx *)
   (*              | SOME s' => *)
   (*                if Sort.eq (s, s') then Ctx.remove (ctx, Left x) *)
   (*                else failwith "sort mismatch" *)
   (*          end *)
   (*        | Top => Ctx.empty *)
   (*    in *)
   (*       {p = pos, n = neg} *)
   (*    end *)

   (* fun close f = *)
   (*    let *)
   (*       fun ffn (x, s, f) = case x of *)
   (*          Left x => All ((x, s), f) *)
   (*        | Right _ => failwith "PFormula.close: found a parameter" *)
   (*    in *)
   (*       Ctx.fold ffn f (PN.neg ctx f) *)
   (*    end *)

   fun rename {old, new} =
      let
         val xy = (old, Term.Var new)
         fun pos f = case f of
            PAtom rel => PAtom (Rel.apply1 (rel, xy))
          | Ex ((x, s), p) =>
            if Var.eq (x, old) then f
            else if Var.eq (x, new) then failwith "renaming captures variables"
            else Ex ((x, s), pos p)
          | Sum (p, q) => Sum (pos p, pos q)
          | Tensor (p, q) => Tensor (pos p, pos q)
          | Down n => Down (neg n)
          | One => One
          | Zero => Zero
          | PLabel (s, p) => PLabel (s, pos p)
         and neg f = case f of
            NAtom rel => NAtom (Rel.apply1 (rel, xy))
          | All ((x, s), p) =>
            if Var.eq (x, old) then f
            else if Var.eq (x, new) then failwith "renaming captures variables"
            else All ((x, s), neg p)
          | With (p, q) => With (neg p, neg q)
          | Lolli (p, n) => Lolli (pos p, neg n)
          | BiLolli (p, n) => BiLolli (neg p, neg n)
          | Up p => Up (pos p)
          | Top => Top
          | NLabel (s, p) => NLabel (s, neg p)
      in
         {p = pos, n = neg}
      end

   fun apply s =
      let
         fun pos f = case f of
            PAtom rel => PAtom (Rel.apply (rel, s))
          | Sum (p, q) => Sum (pos p, pos q)
          | Tensor (p, q) => Tensor (pos p, pos q)
          | Down p => Down (neg p)
          | One => One
          | Zero => Zero
          | Ex ((x, t), p) =>
            let
               val dom = Subst.dom s
               val img = Subst.img s
               val (x, p) =
                  if Atoms.mem (dom, Left x) orelse Atoms.mem (img, Left x) then
                     let
                        val x' = Var.next ()
                        val p = PN.pos (rename {old = x, new = x'}) p
                     in
                        (x', p)
                     end
                  else
                     (x, p)
            in
               Ex ((x, t), pos p)
            end
          | PLabel (s, p) => PLabel (s, pos p)
         and neg f = case f of
            NAtom rel => NAtom (Rel.apply (rel, s))
          | With (p, q) => With (neg p, neg q)
          | Lolli (p, q) => Lolli (pos p, neg q)
          | BiLolli (p, q) => BiLolli (neg p, neg q)
          | Up p => Up (pos p)
          | Top => Top
          | All ((x, t), p) =>
            let
               val dom = Subst.dom s
               val img = Subst.img s
               val (x, p) =
                  if Atoms.mem (dom, Left x) orelse Atoms.mem (img, Left x) then
                     let
                        val x' = Var.next ()
                        val p = PN.neg (rename {old = x, new = x'}) p
                     in
                        (x', p)
                     end
                  else
                     (x, p)
            in
               All ((x, t), neg p)
            end
          | NLabel (s, p) => NLabel (s, neg p)
      in
         {p = pos, n = neg}
      end

   fun apply1 (x, t) =
      let
         val s = Subst.ofList [Left (x, t)]
      in
         {p = pos (apply s), n = neg (apply s)}
      end

   (*** Make bound variables distinct ***)

   val separate =
      let
         fun mem x l = List.genMem Var.eq (x, l)
         fun pos xs = fn
            Tensor (p, q) =>
            let
               val (p, xs) = pos xs p
               val (q, xs) = pos xs q
            in
               (Tensor (p, q), xs)
            end
          | Sum (p, q) =>
            let
               val (p, xs) = pos xs p
               val (q, xs) = pos xs q
            in
               (Sum (p, q), xs)
            end
          | Down p =>
            let
               val (p, xs) = neg xs p
            in
               (Down p, xs)
            end
          | Ex ((x, t), p) =>
            let
               val (p, xs) = pos xs p
            in
               if not (mem x xs)
               then (Ex ((x, t), p), x :: xs)
               else
                  let
                     val x' = Var.next ()
                     val p = PN.pos (rename {old = x, new = x'}) p
                  in
                     (Ex ((x', t), p), x'::xs)
                  end
            end
          | f => (f, xs)
         and neg xs = fn
            With (p, q) =>
            let
               val (p, xs) = neg xs p
               val (q, xs) = neg xs q
            in
               (With (p, q), xs)
            end
          | Lolli (p, q) =>
            let
               val (p, xs) = pos xs p
               val (q, xs) = neg xs q
            in
               (Lolli (p, q), xs)
            end
          | BiLolli (p, q) =>
            let
               val (p, xs) = neg xs p
               val (q, xs) = neg xs q
            in
               (BiLolli (p, q), xs)
            end
          | Up p =>
            let
               val (p, xs) = pos xs p
            in
               (Up p, xs)
            end
          | All ((x, t), p) =>
            let
               val (p, xs) = neg xs p
            in
               if not (mem x xs)
               then (All ((x, t), p), x::xs)
               else
                  let
                     val x' = Var.next ()
                     val p = PN.neg (rename {old = x, new = x'}) p
                  in
                     (All ((x', t), p), x'::xs)
                  end
            end
          | f => (f, xs)
      in
         {p = fst o pos [], n = fst o neg []}
      end

   (* Atoms in Formula.formulas are not polarized.  We thus must give
      some polarity assignment to atoms.  We must do this consistently.
      Thus, we just make every atom negative and let the heuristic
      assign different polarities if it wishes. *)
   val formulate : Formula.t -> neg =
      let
         fun pos f = case f of
            F.Atom rel =>
            let in
               case Rel.sign rel of
                  Pred.Neg => Down (NAtom rel)
                | Pred.Pos => PAtom rel
            end
          | F.Top => One
          | F.Bot => Zero
          | F.And (p, q) => Tensor (pos p, pos q)
          | F.Or (p, q) => Sum (pos p, pos q)
          | F.Ex (x, p) => Ex (x, pos p)
          | _ => Down (neg f)
         and neg f = case f of
            F.Atom rel =>
            let in
               case Rel.sign rel of
                  Pred.Neg => NAtom rel
                | Pred.Pos => Up (PAtom rel)
            end
          | F.Top => Top
          | F.Imp (p, q) => Lolli (pos p, neg q)
          | F.Iff (p, q) => BiLolli (neg p, neg q)
          | F.And (p, q) => With (neg p, neg q)
          | F.Not p => Lolli (pos p, Up Zero)
          | F.All (x, p) => All (x, neg p)
          | _ => Up (pos f)
      in
         neg
      end

   val erase =
      let
         val rec pos = fn
            PAtom m => F.Atom m
          | Tensor (a, b) => F.And (pos a, pos b)
          | One => F.Top
          | Sum (a, b) => F.Or (pos a, pos b)
          | Zero => F.Bot
          | Down n => neg n
          | Ex (x, p) => F.Ex (x, pos p)
          | PLabel (s, a) => F.Label (s, pos a)
         and neg = fn
            NAtom m => F.Atom m
          | With (a, b) => F.And (neg a, neg b)
          | Top => F.Top
          | Lolli (p, n) => F.Imp (pos p, neg n)
          | BiLolli (p, n) => F.Iff (neg p, neg n)
          | Up p => pos p
          | All (x, p) => F.All (x, neg p)
          | NLabel (s, a) => F.Label (s, neg a)
      in
         {p = pos, n = neg}
      end

   val parse : Parse.Formula.t -> neg =
      let
         fun mkRel (rel as P.Rel.R (p, _)) =
            let
               val rel = Rel.parse rel
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
          | PF.Label (s, p) => PLabel (s, pos p)
          | PF.Const P.Const.Top => One
          | PF.Const P.Const.True => One
          | PF.Const P.Const.One => One
          | PF.Const P.Const.False => Zero
          | PF.Const P.Const.Zero => Zero
          | PF.Unop (P.Unop.Down, n) => Down (neg n)
          | PF.Binop (P.Binop.And, p, q) => Tensor (pos p, pos q)
          | PF.Binop (P.Binop.Tensor, p, q) => Tensor (pos p, pos q)
          | PF.Binop (P.Binop.Or, p, q) => Sum (pos p, pos q)
          | PF.Quant (P.Quant.Ex, (x, s), p) =>
            Ex ((Var.ofString x, Sort.Base.parse s), pos p)
          | f => Down (neg f)
         and neg = fn
            PF.Rel rel =>
            let in
               case mkRel rel of
                  Left rel => Up (PAtom rel)
                | Right rel => NAtom rel
            end
          | PF.Label (s, p) => NLabel (s, neg p)
          | PF.Const P.Const.True => Top
          | PF.Const P.Const.Top => Top
          | PF.Unop (P.Unop.Bang, _) => fail ()
          | PF.Unop (P.Unop.Up, p) => Up (pos p)
          | PF.Unop (P.Unop.Not, n) => Lolli (pos n, Up Zero)
          | PF.Binop (P.Binop.With, p, q) => With (neg p, neg q)
          | PF.Binop (P.Binop.Imp, p, q) => Lolli (pos p, neg q)
          | PF.Binop (P.Binop.Lolli, p, q) => Lolli (pos p, neg q)
          | PF.Binop (P.Binop.Imp', p, q) => Lolli (pos q, neg p)
          | PF.Binop (P.Binop.Lolli', p, q) => Lolli (pos q, neg p)
          | PF.Binop (P.Binop.Iff, p, q) => BiLolli (neg p, neg q)
          | PF.Binop (P.Binop.BiLolli, p, q) => BiLolli (neg p, neg q)
          | PF.Quant (P.Quant.All, (x, s), p) =>
            All ((Var.ofString x, Sort.Base.parse s), neg p)
          | f => Up (pos f)
         fun parse f =
            let
               val f = neg f
            in
               if closed f then f else
               failwith "The formula is not closed"
            end
      in
         parse
      end

   fun ofString s = parse (PF.ofString s)

   val signat =
      let
         fun term ctx = fn
            T.Var x =>
            let in
               case Ctx.find (ctx, Left x) of
                  NONE => failwith "Var is missing a sort."
                | SOME s => s
            end
          | T.Param _ => failwith "PFormula.signat: param"
          | T.Fn (f, ts) =>
            let
               val ss = List.map (term ctx) ts
            in
               case S.func S.find f of
                  NONE =>
                  (* If a function symbol doesn't already have its
                     sort defined, the result sort is uninterpreted. *)
                  let
                     val s = Sort.I
                  in
                     PP.ppl (%%[$"Warning: extending by default sort:", Func.pp f, Sort.Func.pp (ss, s)]);
                     S.func S.extend f (ss, s);
                     s
                  end
                | SOME (ss', s) =>
                  let in
                     assert ( fn () => List.all Sort.Base.eq (List.zip (ss, ss'))
                            , fn () => &[ %[$"Unequal sorts for func ", Func.pp f]
                                        , %[\\, $"Expected ", PP.list (List.map Sort.Base.pp ss')]
                                        , %[\\, $"Inferred ", PP.list (List.map Sort.Base.pp ss)]
                                        , %[\\, $"Context  ", Ctx.pp ctx]
                                        , %[\\, $"Signat   ", Signat.pp ()]])
                   ; s
                  end
            end
         fun rel ctx r =
            let
               val (p, ts) = Rel.dest r
               val ss = List.map (term ctx) ts
            in
               case S.pred S.find p of
                  NONE => S.pred S.extend p ss
                | SOME ss' =>
                  assert ( fn () => List.all Sort.Base.eq (List.zip (ss, ss'))
                         , fn () => &[ %[$"Unequal sorts for pred ", Pred.pp p]
                                     , %[\\, $"Rel      ", Rel.pp r]
                                     , %[\\, $"Expected ", PP.list (List.map Sort.Base.pp ss')]
                                     , %[\\, $"Inferred ", PP.list (List.map Sort.Base.pp ss)]
                                     , %[\\, $"Context  ", Ctx.pp ctx]
                                     , %[\\, $"Signat   ", Signat.pp ()]])
            end
         fun pos ctx = fn
            PAtom r => rel ctx r
          | Tensor (a, b) => (pos ctx a; pos ctx b)
          | One => ()
          | Sum (a, b) => (pos ctx a; pos ctx b)
          | Zero => ()
          | Down a => (neg ctx  a)
          | Ex ((x, t), a) => pos (Ctx.extend (ctx, Left x, t)) a
          | PLabel (_, a) => pos ctx a
         and neg ctx = fn
            NAtom r => rel ctx r
          | With (a, b) => (neg ctx a; neg ctx b)
          | Top => ()
          | Lolli (a, b) => (pos ctx a; neg ctx  b)
          | BiLolli (a, b) => (neg ctx  a; neg ctx  b)
          | Up a => (pos ctx a)
          | All ((x, t), a) => neg (Ctx.extend (ctx, Left x, t)) a
          | NLabel (_, a) => neg ctx a
      in
         neg Ctx.empty
      end

   val propositional =
      let
         val rec pos = fn
            PAtom _ => true
          | Tensor (a, b) => pos a andalso pos b
          | One => true
          | Sum (a, b) => pos a andalso pos b
          | Zero => true
          | Down n => neg n
          | Ex _ => false
          | PLabel (_, a) => pos a
         and neg = fn
            NAtom _ => true
          | With (a, b) => neg a andalso neg b
          | Top => true
          | Lolli (p, n) => pos p andalso neg n
          | BiLolli (p, n) => neg p andalso neg n
          | Up p => pos p
          | All _ => false
          | NLabel (_, a) => neg a
      in
         neg
      end

   val () = noWarnUnused (fn _ : 'a func * transform => (map, fold, apply1, ofString))

end
