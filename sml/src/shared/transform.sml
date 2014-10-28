
structure Transform :> Transform = struct
   structure F = PFormula
   structure PSet = Pred.Set

   open General
   open PP.Ops
   open F.Export

   type t = neg

   val clearDoubleShifts =
      let
         val rec pos = fn
            Down (Up p) => pos p
          | Down n => Down (neg n)
          | Tensor (p1, p2) => Tensor (pos p1, pos p2)
          | Sum (p1, p2) => Sum (pos p1, pos p2)
          | Ex (x, p) => Ex (x, pos p)
          | f => f
         and neg = fn
            Up (Down n) => neg n
          | Up p => Up (pos p)
          | With (n1,n2) => With (neg n1, neg n2)
          | Lolli (p1,n2) => Lolli (pos p1, neg n2)
          | BiLolli (n1,n2) => BiLolli (neg n1, neg n2)
          | All (x, p) => All (x, neg p)
          | f => f
      in
         {p = pos, n = neg}
      end

   val clearQuadrupleShifts =
      let
         val rec pos = fn
            Down (Up (Down (Up p))) => pos (Down (Up p))
          | Down n => Down (neg n)
          | Tensor (p1, p2) => Tensor (pos p1, pos p2)
          | Sum (p1, p2) => Sum (pos p1, pos p2)
          | Ex (x, t) => Ex (x, pos t)
          | f => f
         and neg = fn
            Up (Down (Up (Down n))) => neg (Up (Down n))
          | Up p => Up (pos p)
          | With (n1,n2) => With (neg n1, neg n2)
          | Lolli (p1,n2) => Lolli (pos p1, neg n2)
          | BiLolli (n1,n2) => BiLolli (neg n1, neg n2)
          | All (x, p) => All (x, neg p)
          | f => f
      in
         {p = pos, n = neg}
      end

   val tensorize =
      let
         val rec pos = fn
            Tensor (a, b) => Tensor (pos a, pos b)
          | Sum (a, b) => Sum (pos a, pos b)
          | Down a => Down (neg a)
          | Ex (x, a) => Ex (x, pos a)
          | f => f
         and neg = fn
            With (a, b) => Up (Tensor (Down (neg a), Down (neg b)))
          | Lolli (a, b) => Lolli (pos a, neg b)
          | BiLolli (a, b) => BiLolli (neg a, neg b)
          | Up a => Up (pos a)
          | All (x, a) => All (x, neg a)
          | f => f
      in
         {p = pos, n = neg}
      end

   val withize =
      let
         val rec pos = fn
            Tensor (a, b) => Down (With (Up (pos a), Up (pos b)))
          | Sum (a, b) => Sum (pos a, pos b)
          | Down a => Down (neg a)
          | Ex (x, a) => Ex (x, pos a)
          | f => f
         and neg = fn
            With (a, b) => With (neg a, neg b)
          | Lolli (a, b) => Lolli (pos a, neg b)
          | BiLolli (a, b) => BiLolli (neg a, neg b)
          | Up a => Up (pos a)
          | All (x, a) => All (x, neg a)
          | f => f
      in
         {p = pos, n = neg}
      end

   val doubleShiftBiLolli =
      let
         val rec neg = fn
            BiLolli (n1, n2) => Up (Down (BiLolli (neg n1, neg n2)))
          | With (n1, n2) => With (neg n1, neg n2)
          | Lolli (p1, n2) =>
            Lolli (pos p1, neg n2)
          | Up p1 => Up (pos p1)
          | All (x, p) => All (x, neg p)
          | f => f
         and pos = fn
            Tensor (p1, p2) => Tensor (pos p1, pos p2)
          | Sum (p1, p2) => Sum (pos p1, pos p2)
          | Down n1 => Down (neg n1)
          | Ex (x, p) => Ex (x, pos p)
          | f => f
      in
         {p = pos, n = neg}
      end

   (* Add double shifts at the connectives *)
   val singleStep =
      let
         val rec pos = fn
            Tensor (p,q) => F.pos F.shift2 (Tensor (pos p, pos q))
          | Sum (p, q) => F.pos F.shift2 (Sum (pos p, pos q))
          | Down n => Down (neg n)
          | Ex (x, p) => Ex (x, pos p)
          | f => f
         and neg = fn
            With (n, m) => F.neg F.shift2 (With (neg n, neg m))
          | Lolli (p, n) => F.neg F.shift2 (Lolli (pos p, neg n))
          | BiLolli (n, m) => F.neg F.shift2 (BiLolli (neg n, neg m))
          | Up p => Up (pos p)
          | All (x, p) => All (x, neg p)
          | f => f
      in
         neg
      end

   (* Handle exponential blowup of focusing.

      Stabilization and focusing introduces a number of inference
      rules with varying numbers of hypotheses. Unfortunately these
      numbers can be exponential in the size of the formula.
      Return the number of rules that will be generated and the
      maximum number of hypotheses in a rule.

      Exponential explosions can occur when you have sums of products or products
      of sums.  Too little shifting leads to far too many subgoals.  Too much
      shifting leads to a few unsolvable (in a reasonable time) sequents.
      In these cases it seems to be effective to cut the focusing by half.
      A sum of products can also show up as an implication, which ends up
      as a product (morally) on the left.  We set an arbitrary upper bound
      of maxStable for the maximum number of subgoals.  Thus should be
      something around 100000-600000, as we can't hope to do
      more than 1000/second due to overhead from the prover process. *)
   local
      val focusNum =
         let
            val rec neg = fn
               NAtom _ => 1
             | NLabel (_, a) => neg a
             | With (a, b) => neg a + neg b
             | Top => 0
             | Lolli (a, b) => pos a * neg b
             | BiLolli (a, b) => neg a + neg b
             | Up _ => 1
             | All (_, a) => 1 + neg a
            and pos = fn
               PAtom _ => 1
             | PLabel (_, a) => pos a
             | Tensor (a, b) => pos a * pos b
             | One => 1
             | Sum (a, b) => pos a + pos b
             | Zero => 0
             | Down _ => 1
             | Ex (_, a) => 1 + pos a
         in
            {p = pos, n = neg}
         end

      fun exponential f = case f of
         Lolli _ =>
         let in
            let
               val (hyps, body) = F.destLolli f
               val n = F.neg F.size f
            in
               (* It's exponential if the number of premeses is > n^2 *)
               List.all (fn h => F.pos focusNum h > 1) hyps orelse
               Int.prod (map (F.pos focusNum) hyps) > Parameters.Heuristics.maxStable
               orelse F.neg focusNum body > n * n
            end
            handle General.Overflow => true (* Evidently... *)
         end
       | _ => false
   in

      (* Try double-shifting half the hypotheses.  If even this is huge, try
         single stepping to reduce exponential blowup, hoping for a
         contradiction.  *)
      fun doubleShiftExponentials f =
         if not (exponential f)
         then f
         else let
            val (hyps, body) = F.destLolli f
            val n = List.length hyps div 2
            val (hyps1, hyps2) = List.chop (hyps, n)
            val hyps1 = map (F.pos F.shift2) hyps1
         in
            F.mkLolli (hyps1 @ hyps2, body)
         end

      (* a ∧ b ∧ c -> d -> e ---->  ↓↑a ∧ ↓↑b ∧ ↓↑c -> ↓↑d -> e *)
      val shiftHyps =
         let
            val rec pos = fn
               Tensor (p, q) => Tensor (pos p, pos q)
             | f => F.pos F.shift2 f
            and neg = fn
               Lolli (p, q) => Lolli (pos p, neg q)
             | f => f
         in
            {p = pos, n = neg}
         end

      fun shiftExponentials f =
         let
            val f' = doubleShiftExponentials f
         in
            if not (exponential f')
            then f'
            (* If even the double-shifted formula is exponential, doubleshift
               all the hyps, and possibly resort to single stepping *)
            else F.neg shiftHyps f'
         end
   end

   val simplify =
      let
         val neg1 = fn
            With (Up p, Up q) => Up (Tensor (p, q))
          | f => f
         and pos1 = fn
            Tensor (Down p, Down q) => Down (With (p, q))
          | f => f
         val rec neg = fn
            With (p, q) => neg1 (With (neg p, neg q))
          | Lolli (p, q) => neg1 (Lolli (pos p, neg q))
          | BiLolli (p, q) => neg1 (BiLolli (neg p, neg q))
          | Up p => neg1 (Up (pos p))
          | All (x, p) => neg1 (All (x, neg p))
          | f => neg1 f
         and pos = fn
            Tensor (p, q) => pos1 (Tensor (pos p, pos q))
          | Sum (p, q) => pos1 (Sum (pos p, pos q))
          | Down p => pos1 (Down (neg p))
          | Ex (x, p) => pos1 (Ex (x, pos p))
          | f => pos1 f
      in
         F.neg clearDoubleShifts o neg
      end

   val minimal = simplify

   val negAtoms: neg -> neg =
      let
         val rec pos = fn
            PAtom m => Down (NAtom m)
          | PLabel (s, a) => PLabel (s, pos a)
          | Tensor (p1, p2) => Tensor (pos p1, pos p2)
          | One => One
          | Sum (p1, p2) => Sum (pos p1, pos p2)
          | Zero => Zero
          | Down n => Down (neg n)
          | Ex (x, f) => Ex (x, pos f)
         and neg = fn
            NAtom r => NAtom r
          | NLabel (s, a) => NLabel (s, neg a)
          | With (n1, n2) => With (neg n1, neg n2)
          | Top => Top
          | Lolli (p1, n2) => Lolli (pos p1, neg n2)
          | BiLolli (n1, n2) => BiLolli (neg n1, neg n2)
          | Up p => Up (pos p)
          | All (x, f) => All (x, neg f)
      in
         simplify o F.neg clearDoubleShifts o F.neg withize o neg
      end

   val posAtoms: neg -> neg =
      let
         val rec pos = fn
            Tensor (p1, p2) => Tensor (pos p1, pos p2)
          | Sum (p1, p2) => Sum (pos p1, pos p2)
          | Down n => Down (neg n)
          | Ex (x, p) => Ex (x, pos p)
          | f => f
         and neg = fn
            NAtom m => Up (PAtom m)
          | With (n1,n2) => With (neg n1, neg n2)
          | Lolli (p1,n2) => Lolli (pos p1, neg n2)
          | BiLolli (n1,n2) => BiLolli (neg n1, neg n2)
          | Up p => Up (pos p)
          | All (x, p) => All (x, neg p)
          | f => f
      in
         simplify o F.neg clearDoubleShifts o F.neg tensorize o neg
      end

   (* If you have H1..HN ===> A1 & ... & AN, solve A1-AN together.
      Conflicts will make sure they don't interfere with each other,
      and you may be able to save a lot of work. *)
   fun doubleShiftConjunctionInConclusion f =
      let
         val (hyps, body) = F.destLolli f
      in
         case body of
            With _ => F.mkLolli (hyps, F.neg F.shift2 body)
          | _ => f
      end

   val optimize =
      let
         val baseOpt = [ F.neg clearDoubleShifts
                       , F.neg doubleShiftBiLolli
                       , shiftExponentials
                       , doubleShiftConjunctionInConclusion ]
         fun optimizeNeg f =
            List.foldl1 (op o) (negAtoms :: baseOpt) f
         fun optimizePos f =
            List.foldl1 (op o) (posAtoms :: baseOpt) f
         fun opt f =
            let
               val fP = optimizePos f
               val fN = optimizeNeg f
               val sP = F.neg F.size fP
               val sN = F.neg F.size fN
            in
               (* PP.ppl (&[%[$"pos: ", F.pp fP], %[$"neg: ", F.pp fN]]); *)
               if sP < sN then fP else fN
            end
      in
         opt
      end

   fun specialize f =
      let
         val _ =
            if not (F.propositional f)
            then failwith "Calling specialize on non-propositional formula"
            else ()
         val q = Rel.make (Pred.next (), [])
         val {pos, neg} = F.neg F.preds f
         val ats = PSet.union (pos, neg)
         val p_only = PSet.difference (pos, neg)
         val n_only = PSet.difference (neg, pos)
         val ats = PSet.difference (ats, PSet.union (p_only, n_only))
         fun neg keep = fn
            NAtom r =>
            let val p = Rel.pred r in
               if PSet.mem (p_only, p) then Up Zero
               else if PSet.mem (n_only, p) then Top
               else if PSet.mem (keep, p) then NAtom r else Up (PAtom q)
            end
          | With (a, b) => With (neg keep a, neg keep b)
          | Lolli (a, b) => Lolli (pos keep a, neg keep b)
          | BiLolli (a, b) => BiLolli (neg keep a, neg keep b)
          | Up a => Up (pos keep a)
          | f => f
         and pos keep = fn
            PAtom r =>
            let val p = Rel.pred r in
               if PSet.mem (p_only, p) then Zero
               else if PSet.mem (n_only, p) then One
               else if PSet.mem (keep, p) then PAtom r else PAtom q
            end
          | Tensor (a, b) => Tensor (pos keep a, pos keep b)
          | Sum (a, b) => Sum (pos keep a, pos keep b)
          | Down a => Down (neg keep a)
          | f => f
         val keeps =
            let
               val l = PSet.toList ats
               val zero = PSet.empty
               val one = List.map PSet.singleton l
               (* Drop [] and [x] *)
               val more = List.drop (rev (List.tails l), 2)
               val more = List.map PSet.ofList more
            in
               zero :: one @ more
            end
      in
         List.map (fn k => neg k f) keeps
      end

   val () = noWarnUnused (clearQuadrupleShifts)
end
