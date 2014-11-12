
structure SC :> SC = struct
   open General
   open PP.Ops
   structure U = Unicode

   datatype t =
      Init of Rel.t
    | TensorR of t * t
    | TensorL of Rel.t * (Rel.t * Rel.t) * t
    | OneR
    | OneL of Rel.t * t
    | LolliR of Rel.t * t
    | LolliL of Rel.t * (Rel.t * t) * t
    | BiLolliR of (Rel.t * t) * (Rel.t * t)
    | BiLolliL1 of (Rel.t * t) * t * Rel.t
    | BiLolliL2 of (Rel.t * t) * t * Rel.t
    | WithR of t * t
    | WithL1 of Rel.t * (Rel.t * t)
    | WithL2 of Rel.t * (Rel.t * t)
    | PlusR1 of t
    | PlusR2 of t
    | PlusL of Rel.t * (Rel.t * t) * (Rel.t * t)
    | TopR
    | ZeroL of Rel.t
    | DownR of t
    | DownL of Rel.t * (Rel.t * t)
    | UpR of t
    | UpL of Rel.t * (Rel.t * t)
    | AllR of Param.t * t
    | AllL of Rel.t * (Rel.t * Term.t) * t
    | ExR of Term.t * t
    | ExL of Rel.t * (Rel.t * Param.t) * t
    | Hole

   type printable = t

   fun map rfn tfn afn t =
      let
         val f = map rfn tfn afn
      in
         case t of
            Init l => Init (rfn l)
          | TensorR (l, r) => TensorR (f l, f r)
          | TensorL (u, (v, w), d) => TensorL (rfn u, (rfn v, rfn w), f d)
          | OneR => OneR
          | OneL (u, d) => OneL (rfn u, f d)
          | LolliR (u, d) => LolliR (rfn u, f d)
          | LolliL (u, (v, d), d') =>
            LolliL (rfn u, (rfn v, f d), f d')
          | BiLolliR ((u, t1), (v, t2)) =>
            BiLolliR ((rfn u, f t1), (rfn v, f t2))
          | BiLolliL1 ((u, t1), t2, v) => BiLolliL1 ((rfn u, f t1), f t2, rfn v)
          | BiLolliL2 ((u, t1), t2, v) => BiLolliL2 ((rfn u, f t1), f t2, rfn v)
          | WithR (l, r) => WithR (f l, f r)
          | WithL1 (u, (v, d)) => WithL1 (rfn u, (rfn v, f d))
          | WithL2 (u, (v, d)) => WithL2 (rfn u, (rfn v, f d))
          | PlusR1 d => PlusR1 (f d)
          | PlusR2 d => PlusR2 (f d)
          | PlusL (u, (v, d), (v', d')) =>
            PlusL (rfn u, (rfn v, f d), (rfn v', f d'))
          | TopR => TopR
          | ZeroL u => ZeroL (rfn u)
          | DownR d => DownR (f d)
          | DownL (u, (v, d)) => DownL (rfn u, (rfn v, f d))
          | UpR d => UpR (f d)
          | UpL (u, (v, d)) => UpL (rfn u, (rfn v, f d))
          | AllR (a, d) => AllR (afn a, f d)
          | AllL (u, (v, t), d) => AllL (rfn u, (rfn v, tfn t), f d)
          | ExR (t, d) => ExR (tfn t, f d)
          | ExL (u, (v, a), d) => ExL (rfn u, (rfn v, afn a), f d)
          | Hole => Hole
      end

   fun fold rfn tfn afn x t =
      let
         fun f (d, x) = fold rfn tfn afn x d
      in
         case t of
            Init l => rfn (l, x)
          | TensorR (l, r) => f (l, f (r, x))
          | TensorL (u, (v, w), d) => rfn (u, (rfn (v, rfn (w, f (d, x)))))
          | OneR => x
          | OneL (u, d) => rfn (u, f (d, x))
          | LolliR (u, d) => rfn (u, f (d, x))
          | LolliL (u, (v, d), d') => rfn (u, rfn (v, f (d, f (d', x))))
          | BiLolliR ((u, t1), (v, t2)) => rfn (u, f (t1, rfn (v, f (t2, x))))
          | BiLolliL1 ((u, t1), t2, v) => rfn (u, f (t1, f (t2, rfn (v, x))))
          | BiLolliL2 ((u, t1), t2, v) => rfn (u, f (t1, f (t2, rfn (v, x))))
          | WithR (l, r) => f (l, f (r, x))
          | WithL1 (u, (v, d)) => rfn (u, rfn (v, f (d, x)))
          | WithL2 (u, (v, d)) => rfn (u, rfn (v, f (d, x)))
          | PlusR1 d => f (d, x)
          | PlusR2 d => f (d, x)
          | PlusL (u, (v, d), (v', d')) =>
            rfn (u, rfn (v, f (d, rfn (v', f (d', x)))))
          | TopR => x
          | ZeroL u => rfn (u, x)
          | DownR d => f (d, x)
          | DownL (u, (v, d)) => rfn (u, rfn (v, f (d, x)))
          | UpR d => f (d, x)
          | UpL (u, (v, d)) => rfn (u, rfn (v, f (d, x)))
          | AllR (a, d) => afn (a, f (d, x))
          | AllL (u, (v, t), d) => rfn (u, rfn (v, tfn (t, f (d, x))))
          | ExR (t, d) => tfn (t, f (d, x))
          | ExL (u, (v, a), d) => rfn (u, rfn (v, afn (a, f (d, x))))
          | Hole => x
      end

   val atoms =
      fold
         (fn (r, e) => Atoms.union (Rel.atoms r, e))
         (fn (t, e) => Atoms.union (Term.atoms t, e))
         (fn (a, e) => Atoms.add (e, Right a))
         Atoms.empty

   fun apply (t, s) =
      map
         (fn r => Rel.apply (r, s))
         (fn t => Subst.apply (t, s))
         (fn a => Subst.applyP (a, s))
         t

   fun unfix t = map Rel.unfix Term.unfix Param.unfix t

   local
      structure Ctx :>
         sig
            val lookup: Rel.t -> Label.t
            val reset: unit -> unit
            val toList: unit -> (Label.t * Rel.t) list
         end =
         struct
         type t = (Rel.t * Label.t) list ref
         val ctx: t = ref []
         fun reset () = ctx := []
         fun toList () = List.map (fn (x, y) => (y, x)) (!ctx)
         fun lookup r = case List.genAssoc Rel.eq (r, !ctx) of
            SOME b => b
          | NONE =>
            let
               val b = Label.next ()
            in
               ctx := (r, b) :: !ctx
             ; b
            end
      end

      val ` = Ctx.lookup
      fun `` x = ND.Label (`x)
   in
      val reset = Ctx.reset

      val rec nd = fn
         TensorR (D, D') => ND.Pair (nd D, nd D')
       | TensorL (u, (v, v'), D) =>
         ND.Let ((`v, ND.Fst (``u)), ND.Let ((`v', ND.Snd (``u)), nd D))
       | OneR => ND.Unit
       | OneL (u, D) => ND.Let ((`u, ND.Ascribe (ND.Unit, Formula.Top)), nd D)
       | LolliR (u, D) => ND.Lam (`u, nd D)
       | LolliL (u, (v, D), D') => ND.Let ((`v, ND.App (``u, nd D')), nd D)
       | WithR (D, D') => ND.Pair (nd D, nd D')
       | WithL1 (u, (v, D)) => ND.Let ((`v, ND.Fst (``u)), nd D)
       | WithL2 (u, (v, D)) => ND.Let ((`v, ND.Snd (``u)), nd D)
       | PlusR1 D => ND.Inl (nd D)
       | PlusR2 D => ND.Inr (nd D)
       | PlusL (u, (v, D), (v', D')) => ND.Case (``u, (`v, nd D), (`v', nd D'))
       | TopR => ND.Unit
       | ZeroL u => ND.Abort (``u)
       | DownR D => nd D
       | DownL (u, (v, D)) => ND.Let ((`v, ``u), nd D)
       | UpR D => nd D
       | UpL (u, (v, D)) => ND.Let ((`v, ``u), nd D)
       | Init u => if Rel.isConstr u then ND.Hole else ND.Elim (``u)
       | BiLolliR ((v1, D1), (v2, D2)) =>
         ND.Pair (ND.Lam (`v1, nd D1), ND.Lam (`v2, nd D2))
       | BiLolliL1 ((u, D), D1, v) =>
         ND.Let ((`u, ND.App (ND.Fst (``v), nd D1)), nd D)
       | BiLolliL2 ((u, D), D2, v) =>
         ND.Let ((`u, ND.App (ND.Snd (``v), nd D2)), nd D)
       | AllR (a, D) =>
         let
            val x = Var.next ()
            val M = ND.paramSubst (nd D, (a, x))
         in
            ND.QLam (x, M)
         end
       | AllL (u, (v, t), D) => ND.Let ((`v, ND.QApp (``u, t)), nd D)
       | ExR (t, D) => ND.Pack (t, nd D)
       | ExL (u, (v, a), D) =>
         let
            val x = Var.next ()
            val M = ND.paramSubst (nd D, (a, x))
         in
            ND.Unpack (x, `v, ``u, M)
         end
       | Hole => failwith "SC.nd"

      val nd1 = nd

      fun nd {norm} t =
         let
            val t = map norm Fun.id Fun.id t
            val tm = nd1 t
            val ctx = Ctx.toList ()
         in
            (tm, ctx)
         end
   end

   fun thaw fs = fn
      Init l => Init (Rel.thaw fs l)
    | TensorR (l, r) => TensorR (thaw fs l, thaw fs r)
    | TensorL (u, (v, w), d) =>
      TensorL (Rel.thaw fs u,
               (Rel.thaw fs v,
                Rel.thaw fs w),
               thaw fs d)
    | OneR => OneR
    | OneL (u, d) => OneL (Rel.thaw fs u, thaw fs d)
    | LolliR (u, d) => LolliR (Rel.thaw fs u, thaw fs d)
    | LolliL (u, (v, d), d') =>
      LolliL (Rel.thaw fs u,
              (Rel.thaw fs v, thaw fs d),
              thaw fs d')
    | BiLolliR ((u, t1), (v, t2)) =>
      BiLolliR ((Rel.thaw fs u, thaw fs t1), (Rel.thaw fs v, thaw fs t2))
    | BiLolliL1 ((u, t1), v, t2) =>
      BiLolliL1 ((Rel.thaw fs u, thaw fs t1), thaw fs v, Rel.thaw fs t2)
    | BiLolliL2 ((u, t1), v, t2) =>
      BiLolliL2 ((Rel.thaw fs u, thaw fs t1), thaw fs v, Rel.thaw fs t2)
    | WithR (l, r) => WithR (thaw fs l, thaw fs r)
    | WithL1 (u, (v, d)) =>
      WithL1 (Rel.thaw fs u, (Rel.thaw fs v, thaw fs d))
    | WithL2 (u, (v, d)) =>
      WithL2 (Rel.thaw fs u, (Rel.thaw fs v, thaw fs d))
    | PlusR1 d => PlusR1 (thaw fs d)
    | PlusR2 d => PlusR2 (thaw fs d)
    | PlusL (u, (v, d), (v', d')) =>
      PlusL (Rel.thaw fs u,
             (Rel.thaw fs v, thaw fs d),
             (Rel.thaw fs v', thaw fs d'))
    | TopR => TopR
    | ZeroL u => ZeroL (Rel.thaw fs u)
    | DownR d => DownR (thaw fs d)
    | DownL (u, (v, d)) =>
      DownL (Rel.thaw fs u, (Rel.thaw fs v, thaw fs d))
    | UpR d => UpR (thaw fs d)
    | UpL (u, (v, d)) =>
      UpL (Rel.thaw fs u, (Rel.thaw fs v, thaw fs d))
    | AllR (a, d) => AllR (a, thaw fs d)
    | AllL (u, (v, t), d) =>
      let
         val u' = Rel.thaw fs u
         val v' = Rel.thaw fs v
         val t' = Term.thaw fs t
         val d' = thaw fs d
      in
         AllL (u', (v', t'), d')
      end
    | ExR (t, d) => ExR (Term.thaw fs t, thaw fs d)
    | ExL (u, (v, a), d) =>
      ExL (Rel.thaw fs u,
           (Rel.thaw fs v,
            a),
           thaw fs d)
    | Hole => failwith "ND.thaw"

   local
      val ` = Rel.pp
      val com = %[PP.comma, \]
      val com1 = PP.comma
      val lp = PP.lparen
      val rp = PP.rparen
   in
      val rec pp = fn
         TensorR (t1, t2) : printable =>
         PP.hang (%[$(U.wedge^"R"), lp]) 2
            (PP.cat [%[pp t1, com1], %[pp t2, rp]])
       | TensorL (u, (v, w), d) =>
         PP.hang (%[$(U.wedge^"L"), lp, `u, com, lp, `v, com, `w, rp, com1]) 2
            (%[pp d, rp])
       | OneR => $"1R"
       | OneL (u, t) => PP.hang (%[$"1L (", `u, com1]) 2 (%[pp t, rp])
       | LolliR (u, t) => PP.hang (%[$"-oR (",`u, com1]) 2 (%[pp t, rp])
       | LolliL (u, (v, t), t') =>
         PP.hang (%[$"-oL (", `u, com, lp, `v, com1]) 2
            (PP.cat [%[pp t, rp, com], %[pp t', rp]])
       | BiLolliR ((u, t1), (v, t2)) =>
         PP.hang (%[$"o-oR", lp, lp, `u, com1]) 2
            (PP.cat [%[pp t1, rp, com, lp, `v, com1], %[pp t2, rp, rp]])
       | BiLolliL1 ((v,t), t', w) =>
         PP.hang (%[$"o-oL1 ((",`v, com1]) 2
            (PP.cat [%[pp t, rp, com1], %[pp t', com, `w, rp]])
       | BiLolliL2 ((v,t), t', w) =>
         PP.hang (%[$"o-oL2 ((",`v, com1]) 2
            (PP.cat [%[pp t, rp, com1], %[pp t', com, `w, rp]])
       | WithR (t1, t2) =>
         PP.hang ($"&R (") 2 (PP.cat [%[pp t1, com1], %[pp t2, rp]])
       | WithL1 (u, (v,t)) =>
         PP.hang (%[$"&L1 (", `u, com, lp, `v, com1]) 2 (%[pp t, rp])
       | WithL2 (u, (v,t)) =>
         PP.hang (%[$"&L2 (", `u, com, lp, `v, com1]) 2 (%[pp t, rp])
       | PlusR1 t => PP.hang (%[$(U.vee^"R1"), lp]) 2 (%[pp t, rp])
       | PlusR2 t => PP.hang (%[$(U.vee^"R2"), lp]) 2 (%[pp t, rp])
       | PlusL (u, (v,t), (v',t')) =>
         PP.hang (%[$(U.vee^"L"), lp, `u, com, lp, `v, com1]) 2
            (PP.cat [%[pp t, rp, com, lp, `v', com1], %[pp t', rp, rp]])
       | TopR => $"TR"
       | ZeroL v => %[$"0L ", `v]
       | DownR t => %[$(U.down^"R"), lp, pp t, rp]
       | DownL (u, (v,t)) =>
         PP.hang (%[$(U.down^"L"), lp, `u, com, lp, `v, com1]) 2 (%[pp t, rp, rp])
       | UpR t => %[$(U.up^"R"), lp, pp t, rp]
       | UpL (u, (v,t)) =>
         PP.hang (%[$(U.up^"L"), lp, `u, com, lp, `v, com1]) 2 (%[pp t, rp, rp])
       | Init v => %[$"I ", `v]
       | AllR (p, t) =>
         PP.hang (%[$"AllR (", Param.pp p, com1]) 2 (%[pp t, rp])
       | AllL (u, (v, t), t') =>
         PP.hang (%[$"AllL (", `u, com, lp, `v, com, Term.pp t, rp, com1]) 2
            (%[pp t',rp])
       | ExR (t,t') =>
         PP.hang (%[$"ExR (", Term.pp t, com1]) 2 (%[pp t', rp])
       | ExL (u, (v, a), t) =>
         PP.hang (%[$"ExL (", `u, com, lp, `v, com
                   , Param.pp a, rp, com1]) 2 (%[pp t, rp])
       | Hole => $U.circ
   end
end
