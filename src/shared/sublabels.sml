
structure Sublabels :> Sublabels = struct
   structure Map = Pred.Map
   structure Set = Pred.Set
   structure F = LFormula

   open General
   open PP.Ops

   type t = Pred.set Pred.map
   type printable = t
   fun lookup (t:t, l) = Option.getOpt (Map.find (t, l), Set.empty)
   val lp = F.pos F.pred
   val ln = F.neg F.pred

   fun make f =
      let
         val union = Map.unionWith (fn _ => failwith "Sublabels.union")
         fun mk1 l (l1, m1) = Map.insert (m1, l, Set.add (lookup (m1, l1), l))
         fun mk2 l (l1, m1) (l2, m2) =
            let
               val s = Set.add (Set.union (lookup (m1, l1), lookup (m2, l2)), l)
            in
               Map.insert (union (m1, m2), l, s)
            end
         fun neg f k =
            let
               val l = ln f
            in
               case F.neg F.expose f of
                  F.With (a, b) =>
                  neg a
                     (fn m1 => neg b
                                  (fn m2 =>
                                     k (mk2 l (ln a, m1) (ln b, m2))))
                | F.Top => k Map.empty
                | F.NAtom _ => k Map.empty
                | F.BiLolli (a, b) =>
                  neg a
                     (fn m1 => neg b
                                  (fn m2 => k (mk2 l (ln a, m1) (ln b, m2))))
                | F.Lolli (a, b) =>
                  pos a
                     (fn m1 => neg b
                                  (fn m2 => k (mk2 l (lp a, m1) (ln b, m2))))
                | F.Up a => pos a (fn m => k (mk1 l (lp a, m)))
                | F.All (_, a) => neg a (fn m => k (mk1 l (ln a, m)))
            end
         and pos f k =
            let
               val l = lp f
            in
               case F.pos F.expose f of
                  F.Tensor (a, b) =>
                  pos a
                     (fn m1 => pos b
                                  (fn m2 =>
                                     k (mk2 l (lp a, m1) (lp b, m2))))
                | F.Sum (a, b) =>
                  pos a
                     (fn m1 => pos b
                                  (fn m2 =>
                                     k (mk2 l (lp a, m1) (lp b, m2))))
                | F.PAtom _ => k Map.empty
                | F.One => k Map.empty
                | F.Zero => k Map.empty
                | F.Down a => neg a (fn m => k (mk1 l (ln a, m)))
                | F.Ex (_, a) => pos a (fn m => k (mk1 l (lp a, m)))
            end
      in
         neg f (fn k => k)
      end

   fun pp (t:printable) =
      &[ $"Sublabels",
         %[\\, Map.ppVert (fn (p, ps) =>
                              PP.hsep [Pred.pp p, Pred.Set.pp ps]) t]]
   val () = noWarnUnused (pp)
end
