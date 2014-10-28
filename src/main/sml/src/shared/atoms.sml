
structure Atoms :> Atoms = struct
   structure VSet = Var.Set
   structure PSet = Param.Set
   structure U = Unicode

   open General
   open PP.Ops

   type t = Var.set * Param.set

   type eqable = t
   type printable = t

   val empty = (VSet.empty, PSet.empty)

   val make = Fun.id
   val dest = Fun.id
   val vars = fst
   val params = snd

   fun size (x, y) = VSet.size x + PSet.size y

   fun add ((xs, ps), at) = case at of
      Left x => (VSet.add (xs, x), ps)
    | Right a => (xs, PSet.add (ps, a))

   fun remove ((xs, ps), at) = case at of
      Left x => (VSet.remove (xs, x), ps)
    | Right a => (xs, PSet.remove (ps, a))

   fun singleton x = add (empty, x)

   fun mem ((xs, ps), at) = case at of
      Left x => VSet.mem (xs, x)
    | Right a => PSet.mem (ps, a)

   fun union ((x1, p1), (x2, p2)) =
      (VSet.union (x1, x2), PSet.union (p1, p2))

   fun disjoint ((x1, p1), (x2, p2)) =
      VSet.disjoint (x1, x2) andalso PSet.disjoint (p1, p2)

   val unions = foldl union empty

   fun intersection ((x1, p1), (x2, p2)) =
      (VSet.intersection (x1, x2), PSet.intersection (p1, p2))

   fun difference ((x1, p1), (x2, p2)) =
      (VSet.difference (x1, x2), PSet.difference (p1, p2))

   fun isEmpty (xs, ps) = VSet.isEmpty xs andalso PSet.isEmpty ps

   fun eq ((vs1, ps1), (vs2, ps2)) =
      VSet.eq (vs1, vs2) andalso PSet.eq (ps1, ps2)

   fun subset ((vs1, ps1), (vs2, ps2)) =
      VSet.subset (vs1, vs2) andalso PSet.subset (ps1, ps2)

   fun fixed (vs, ps) =
      ( VSet.map Var.unfix (VSet.filter Var.isFixed vs)
      , PSet.map Param.unfix (PSet.filter Param.isFixed ps))

   fun pp (vs, ps) = %[$U.langle, VSet.pp vs, $", ", PSet.pp ps, $U.rangle]

   fun fix ((vs, ps), fresh) =
      (vs, Param.Set.map
              (fn p => if PSet.mem (fresh, p) then Param.fix p else p) ps)

   fun fold f x (vs, ps) =
      VSet.foldr (fn (x, acc) => f (Left x, acc))
         (PSet.foldr (fn (a, acc) => f (Right a, acc)) x ps) vs

   fun all f (vs, ps) =
      VSet.all (f o Left) vs andalso PSet.all (f o Right) ps

   fun unfix (vs, ps) = (VSet.map Var.unfix vs, PSet.map Param.unfix ps)

   val () =
      noWarnUnused
         (fn _ : eqable * printable =>
             (intersection, difference, isEmpty, pp, vars, params
             , disjoint, size, all))
end
