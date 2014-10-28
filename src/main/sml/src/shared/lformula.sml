
structure LFormula :> LFormula = struct
   structure F = PFormula
   structure T = Term
   structure S = Signat

   open General
   open PP.Ops

   datatype pos' =
      PAtom of Rel.t
    | Tensor of pos * pos
    | One
    | Sum of pos * pos
    | Zero
    | Ex of (Var.t * Sort.t) * pos
    | Down of neg

   and neg' =
      NAtom of Rel.t
    | With of neg * neg
    | Top
    | Lolli of pos * neg
    | BiLolli of neg * neg
    | All of (Var.t * Sort.t) * neg
    | Up of pos

   withtype pos = Rel.t * pos'
        and neg = Rel.t * neg'

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

   val expose = {p = snd, n = snd}
   val label = {p = fst, n = fst}
   val pred : Pred.t func =
      {p = Rel.pred o pos label, n = Rel.pred o neg label}

   fun make f =
      let
         val _ = F.signat f
         fun gen ctx =
            let
               fun ffn (x, s, (xs, ss)) =
                  case x of
                     Right _ => failwith "param in lformula"
                   | Left x => (x :: xs, s :: ss)
               val (xs, ss) = Ctx.fold ffn ([], []) ctx
               val p = Pred.next ()
               val _ = S.pred S.extend p ss
            in
               Rel.make (p, map T.Var xs)
            end
         fun pos ctx = fn
            F.PAtom rel => (rel, PAtom rel)
          | F.One => (Rel.one, One)
          | F.PLabel (_, f) => pos ctx f
          | F.Zero => (Rel.zero, Zero)
          | f =>
            let
               val ats = F.pos F.atoms f
               val rel = gen (Ctx.restrict (ctx, ats))
            in
               case f of
                  F.Tensor (p, q) =>
                  (rel, Tensor (pos ctx p, pos ctx q))
                | F.Sum (p, q) => (rel, Sum (pos ctx p, pos ctx q))
                | F.Down p => (rel, Down (neg ctx p))
                | F.Ex ((x, t), p) =>
                  let
                     val ctx = Ctx.extend (ctx, Left x, t)
                  in
                     (rel, Ex ((x, t), pos ctx p))
                  end
                | _ => failwith "LFormula.make (pos)"
            end
         and neg ctx = fn
            F.NAtom rel => (rel, NAtom rel)
          | F.Top => (Rel.top, Top)
          | F.NLabel (_, f) => neg ctx f
          | f =>
            let
               val ats = F.neg F.atoms f
               val rel = gen (Ctx.restrict (ctx, ats))
            in
               case f of
                  F.With (p, q) =>
                  (rel, With (neg ctx p, neg ctx q))
                | F.Lolli (p, q) =>
                  (rel, Lolli (pos ctx p, neg ctx q))
                | F.BiLolli (p, q) =>
                  (rel, BiLolli (neg ctx p, neg ctx q))
                | F.Up p => (rel, Up (pos ctx p))
                | F.All ((x, t), p) =>
                  let
                     val ctx = Ctx.extend (ctx, Left x, t)
                  in
                     (rel, All ((x, t), neg ctx p))
                  end
                | _ => let in
                   PP.ppl (&[$"Can't parse formula", ~, F.neg F.pp f]);
                   failwith "LFormula.make (neg)"
                end
            end
      in
         neg Ctx.empty f
      end

   val pformula =
      let
         fun pos (_, f) =
            case f of
               PAtom rel => F.PAtom rel
             | Tensor (p, q) => F.Tensor (pos p, pos q)
             | One => F.One
             | Sum (p, q) => F.Sum (pos p, pos q)
             | Zero => F.Zero
             | Ex (x, p) => F.Ex (x, pos p)
             | Down p => F.Down (neg p)
         and neg (_, f) =
            case f of
               NAtom rel => F.NAtom rel
             | With (p, q) => F.With (neg p, neg q)
             | Top => F.Top
             | Lolli (p, q) => F.Lolli (pos p, neg q)
             | BiLolli (p, q) => F.BiLolli (neg p, neg q)
             | All (x, p) => F.All (x, neg p)
             | Up p => F.Up (pos p)
      in
         {p = pos, n = neg}
      end

   val ppLabels =
      let
         val ppP = Rel.pp o PN.pos label
         val ppN = Rel.pp o PN.neg label
         fun pos f =
            let
               val l = Rel.pp (PN.pos label f)
            in
               case PN.pos expose f of
                  Tensor (p, q) =>
                  let
                     val lps = pos p
                     val lqs = pos q
                     val r = %%[ppP p, $Unicode.wedge, ppP q]
                  in
                     (l, r) :: lps @ lqs
                  end
                | Sum (p, q) =>
                  let
                     val lps = pos p
                     val lqs = pos q
                     val r = %%[ppP p, $Unicode.vee, ppP q]
                  in
                     (l, r) :: lps @ lqs
                  end
                | Down p =>
                  let
                     val lps = neg p
                     val r = %[$Unicode.down, ppN p]
                  in
                     (l, r) :: lps
                  end
                | Ex ((x, t), p) =>
                  let
                     val lps = pos p
                     val t = if Sort.Base.isInd t then PP.empty else %[$":", Sort.Base.pp t]
                     val r = %[$Unicode.exists, \, Var.pp x, t, $".", \, ppP p]
                  in
                     (l, r) :: lps
                  end
                | _ => []
            end
         and neg f =
            let
               val l = Rel.pp (PN.neg label f)
            in
               case PN.neg expose f of
                  With (p, q) =>
                  let
                     val lps = neg p
                     val lqs = neg q
                     val r = %%[ppN p, $"&", ppN q]
                  in
                     (l, r) :: lps @ lqs
                  end
                | Lolli (p, q) =>
                  let
                     val lps = pos p
                     val lqs = neg q
                     val r = %%[ppP p, $"-o", ppN q]
                  in
                     (l, r) :: lps @ lqs
                  end
                | BiLolli (p, q) =>
                  let
                     val lps = neg p
                     val lqs = neg q
                     val r = %%[ppN p, $"o-o", ppN q]
                  in
                     (l, r) :: lps @ lqs
                  end
                | Up p =>
                  let
                     val lps = pos p
                     val r = %[$Unicode.up, ppP p]
                  in
                     (l, r) :: lps
                  end
                | All ((x, t), p) =>
                  let
                     val lps = neg p
                     val t = if Sort.Base.isInd t then PP.empty else %[$":", Sort.Base.pp t]
                     val r = PP.cat [$Unicode.all, \, Var.pp x, t, $".", \, ppN p]
                  in
                     (l, r) :: lps
                  end
                | _ => []
            end
         fun label f =
            &[ $"Labels",
              %[\\, &(map (fn (l, p) => %%[l, $"=", p]) (neg f))]]
      in
         label
      end

   fun lift f =
      let
         fun pos p = F.pos f (PN.pos pformula p)
         fun neg p = F.neg f (PN.neg pformula p)
      in
         {p = pos, n = neg}
      end

   val erase = lift F.erase
   val size = lift F.size
   val pp = lift F.pp
   val atoms = lift F.atoms
   (* val ctx = lift F.ctx *)
   val preds = lift F.preds
   fun propositional f = F.propositional (PN.neg pformula f)

   fun rename (old, new) =
      let
         val xy = (old, Term.Var new)
         fun pos (rel, f) =
            let
               val rel = Rel.apply1 (rel, xy)
               val f = case f of
                  PAtom _ => PAtom rel
                | Sum (p, q) => Sum (pos p, pos q)
                | Tensor (p, q) => Tensor (pos p, pos q)
                | Down n => Down (neg n)
                | One => One
                | Zero => Zero
                | Ex ((x, s), p) =>
                  if Var.eq (x, old) then f
                  else if Var.eq (x, new) then failwith "renaming captures variables"
                  else Ex ((x, s), pos p)
            in
               (rel, f)
            end
         and neg (rel, f) =
            let
               val rel = Rel.apply1 (rel, xy)
               val f = case f of
                  NAtom _ => NAtom rel
                | With (p, q) => With (neg p, neg q)
                | Lolli (p, n) => Lolli (pos p, neg n)
                | BiLolli (p, n) => BiLolli (neg p, neg n)
                | Up p => Up (pos p)
                | Top => Top
                | All ((x, s), p) =>
                  if Var.eq (x, old) then f
                  else if Var.eq (x, new) then failwith "renaming captures variables"
                  else All ((x, s), neg p)
            in
               (rel, f)
            end
      in
         {p = pos, n = neg}
      end

   fun apply s =
      let
         fun pos (rel, f) =
            let
               val rel = Rel.apply (rel, s)
               val f = case f of
                  PAtom _ => PAtom rel
                | Tensor (p, q) => Tensor (pos p, pos q)
                | One => One
                | Sum (p, q) => Sum (pos p, pos q)
                | Zero => Zero
                | Down p => Down (neg p)
                | Ex ((x, t), p) =>
                  let
                     val dom = Subst.dom s
                     val img = Subst.img s
                     val (x, p) =
                        if Atoms.mem (dom, Left x)
                           orelse Atoms.mem (img, Left x)
                        then
                           let
                              val x' = Var.next ()
                              val p = PN.pos (rename (x, x')) p
                           in
                              (x', p)
                           end
                        else
                           (x, p)
                  in
                     Ex ((x, t), pos p)
                  end
            in
               (rel, f)
            end
         and neg (rel, f) =
            let
               val rel = Rel.apply (rel, s)
               val f = case f of
                  NAtom _ => NAtom rel
                | With (p, q) => With (neg p, neg q)
                | Top => Top
                | Lolli (p, q) => Lolli (pos p, neg q)
                | BiLolli (p, q) => BiLolli (neg p, neg q)
                | Up p => Up (pos p)
                | All ((x, t), p) =>
                  let
                     val dom = Subst.dom s
                     val img = Subst.img s
                     val (x, p) =
                        if Atoms.mem (dom, Left x)
                           orelse Atoms.mem (img, Left x)
                        then
                           let
                              val x' = Var.next ()
                              val p = PN.neg (rename (x, x')) p
                           in
                              (x', p)
                           end
                        else
                           (x, p)
                  in
                     All ((x, t), neg p)
                  end
            in
               (rel, f)
            end
      in
         {p = pos, n = neg}
      end

   fun apply1 (x, t) =
      let
         val s = Subst.ofList [Left (x, t)]
      in
         {p = pos (apply s), n = neg (apply s)}
      end

   val freeze =
      let
         fun pos (rel, f) =
            let
               val rel = Rel.freeze rel
            in
               case f of
                  PAtom _ => (rel, PAtom rel)
                | Tensor (p, q) => (rel, Tensor (pos p, pos q))
                | One => (rel, One)
                | Sum (p, q) => (rel, Sum (pos p, pos q))
                | Zero => (rel, Zero)
                | Ex (x, p) => (rel, Ex (x, pos p))
                | Down p => (rel, Down (neg p))
            end
         and neg (rel, f) =
            let
               val rel = Rel.freeze rel
            in
               case f of
                  NAtom _ => (rel, NAtom rel)
                | With (p, q) => (rel, With (neg p, neg q))
                | Top => (rel, Top)
                | Lolli (p, q) => (rel, Lolli (pos p, neg q))
                | BiLolli (p, q) => (rel, BiLolli (neg p, neg q))
                | All (x, p) => (rel, All (x, neg p))
                | Up p => (rel, Up (pos p))
            end
      in
         {p = pos, n = neg}
      end

   type unlabel = Rel.t -> (pos, neg) Either.t

   structure Unlabel : sig
      val unlabel: neg -> Rel.t -> (pos, neg) Either.t
   end = struct
      structure T = Pred.Table

      type t = (pos, neg) either T.t

      val apply = fn (f, s) => case f of
         Left p => Left (PN.pos (apply s) p)
       | Right p => Right (PN.neg (apply s) p)

      val rel = fn
         Left (r, _) => r
       | Right (r, _) => r

      val pp1 = pp
      val pp  = fn (_, f) => case f of
         Left f => %[Rel.pp (PN.pos label f), $" : ", PN.pos pp f]
       | Right f => %[Rel.pp (PN.neg label f), $" : ", PN.neg pp f]

      fun unlabel f : unlabel =
         let
            val t : t = T.create (PN.neg size f, NotFound)
            val _ = let in
               T.insertExn t (Pred.zero, Left (Rel.zero, Zero))
             ; T.insertExn t (Pred.one, Left (Rel.one, One))
             ; T.insertExn t (Pred.top, Right (Rel.top, Top))
            end
            fun pos (r, f) =
               let
                  val p = Rel.pred r
                  val singular = fn
                     PAtom _ => true
                   | Zero => true
                   | One => true
                   | _ => false
               in
                  if T.inDomain t p then
                     let in
                        assert
                           (fn () => singular f,
                            fn () => %[$"unlabel: duplicate non-atom: ", PN.pos pp1 (r, f)])
                      ; ()
                     end
                  else T.insertExn t (p, Left (r, f))
                ; case f of
                     PAtom _ => ()
                   | Tensor (p, q) => (pos p; pos q)
                   | Sum (p, q) => (pos p; pos q)
                   | Down p => neg p
                   | Ex (_, p) => pos p
                   | One => ()
                   | Zero => ()
               end
            and neg (r, f) =
               let
                  val p = Rel.pred r
                  val singular = fn
                     NAtom _ => true
                   | Top => true
                   | _ => false
               in
                  if T.inDomain t p then
                     let in
                        assert
                           (fn () => singular f,
                            fn () => %[$"unlabel: duplicate non-atom: ", PN.neg pp1 (r, f)])
                      ; ()
                     end
                  else T.insertExn t (p, (Right (r, f)))
                ; case f of
                     NAtom _ => ()
                   | With (p, q) => (neg p; neg q)
                   | Lolli (p, q) => (pos p; neg q)
                   | BiLolli (p, q) => (neg p; neg q)
                   | Up p => pos p
                   | All (_, p) => neg p
                   | Top => ()
               end
            val _ = neg f
            val _ = Log.trace (fn () =>
               &[ $"Unlabel:"
                , %[\\, T.pp pp t]])
         in
            fn r =>
               let
                  val (p, ts) = Rel.dest r
                  val rf = case Pred.Table.find t p of
                     NONE => failwith' (%[$"unlabel: can't find pred: ", Pred.pp p])
                   | SOME rf => rf
                  val (r', f) = case rf of
                     Left (r', f) => (r', Left f)
                   | Right (r', f) => (r', Right f)
               in
                  case f of
                     Left (PAtom _) => Left (r, PAtom r)
                   | Right (NAtom _) => Right (r, NAtom r)
                   | _ =>
                     let
                        val (_, ts') = Rel.dest r'
                        val ts = List.zip (ts', ts)
                        val ts = List.map (fn
                           (Term.Var x, t) => Left (x, t)
                         | (Term.Param a, Term.Param b) => Right (a, b)
                         | _ => failwith'
                                   (%[$"unlabel mismatch: ",
                                      PP.list (map (fn (t1, t2) => PP.pair (Term.pp t1, Term.pp t2)) ts)]))
                                    ts
                        val s = Subst.ofList ts
                        val rf = apply (rf, s)
                     in
                        asserts (fn () => Rel.eq (r, rel rf), "Rels not equal after substitution.")
                      ; rf
                     end
               end
         end
   end
   open Unlabel

   val () = noWarnUnused (preds)
end
