
structure Conflicts :> Conflicts = struct
   structure F = LFormula
   structure Map = Pred.Map
   structure Set = Pred.Set
   structure Sub = Sublabels

   open General
   open PP.Ops

   type t = Pred.set Pred.map
   type printable = t

   datatype occ = Pos of F.pos | Neg of F.neg

   val lp = F.pos F.pred
   val ln = F.neg F.pred

   fun allowed (t, preds) =
      let
         fun lookup l = Option.getOpt (Map.find (t, l), Set.empty)
         val conflict = Pred.Set.foldl (fn (p, acc) =>
            Pred.Set.union (lookup p, acc))
                           Pred.Set.empty preds
      in
         not (Pred.Set.exists (fn s => Pred.Set.mem (conflict, s)) preds)
      end

   val dummy : t = Map.empty

   fun restrict (m, s) =
      let
         val m = Map.filteri (fn (p, _) => Set.mem (s, p)) m
      in
         Map.map (fn s' => Set.intersection (s, s')) m
      end

   fun conflictPairs f : (Pred.t * Pred.t * Pred.t) list =
      let
         fun loopL acc = fn
            [] => acc
          | Neg p :: rest =>
            let in
               case F.neg F.expose p of
                  F.Up p => loopL acc (Pos p :: rest)
                | _ => loopL acc rest
            end
          | Pos p :: rest =>
            let in
               case F.pos F.expose p of
                  F.Down p => loopL acc (Neg p :: rest)
                | F.Sum (p1, p2) =>
                  loopL
                     ((lp p, lp p1, lp p2) :: acc)
                     (Pos p1 :: Pos p2 :: rest)
                | _ => loopL acc rest
            end
         fun loopR (acc, lefts) = fn
            [] => loopL acc lefts
          | Neg p :: rest =>
            let in
               case F.neg F.expose p of
                  F.Up p => loopR (acc, lefts) (Pos p :: rest)
                | F.With (p1, p2) =>
                  loopR
                     ((ln p, ln p1, ln p2) :: acc, lefts)
                     (Neg p1 :: Neg p2 :: rest)
                | F.Lolli (p1, p2) =>
                  loopR (acc, Pos p1 :: lefts) (Neg p2 :: rest)
                | _ => loopR (acc, lefts) rest
            end
          | Pos p :: rest =>
            let in
               case F.pos F.expose p of
                  F.Down p => loopR (acc, lefts) (Neg p :: rest)
                | F.Sum (p1, p2) =>
                  loopR
                     ((lp p, lp p1, lp p2) :: acc, lefts)
                     (Pos p1 :: Pos p2 :: rest)
                | F.Tensor (p1, p2) =>
                  loopR
                     ((lp p, lp p1, lp p2) :: acc, lefts)
                     (Pos p1 :: Pos p2 :: rest)
                | _ => loopR (acc, lefts) rest
            end
      in
         loopR ([], []) [Neg f]
      end

   (* Uniquely occurring formulas. *)
   fun make (sub, f) =
      let
         val pairs = conflictPairs f
         fun add (m, l, s) =
            Map.change (fn NONE => SOME s | SOME s' => SOME (Set.union (s, s'))) (m, l)
         fun conflict ((parent, left, right), m) =
            let
               val sl = Sub.lookup (sub, left)
               val sr = Sub.lookup (sub, right)
               val m = add (m, parent, Set.union (sl, sr))
               val m = add (m, left, sr)
               val m = add (m, right, sl)
            in
               m
            end
      in
         List.foldl conflict Map.empty pairs
      end

   fun pp t =
      let
         val t = Map.filter (not o Pred.Set.isEmpty) t
      in
         &[ $"Conflicts"
          , %[\\, Map.ppVert
                     (fn (p, ps) =>
                         PP.hsep [Pred.pp p, Pred.Set.pp ps]) t]]

      end

   val () = noWarnUnused (fn _ : printable => ())

end
