
structure Ctx :> Ctx = struct
   structure VSet = Var.Set
   structure PSet = Param.Set
   structure T = Term
   structure S = Signat

   open General
   open PP.Ops

   structure Map = Atom.Map

   type t = Sort.t Map.t
   type printable = t

   val empty = Map.empty

   fun fix (m, fresh) =
      let
         val ats = Atoms.make (VSet.empty, fresh)
         fun ffn (a, s, m) =
            let
               fun ins a = case Map.find (m, a) of
                  NONE => Map.insert (m, a, s)
                | SOME _ => failwith "Ctx.fix"
            in
               if Atoms.mem (ats, a)
               then ins (Atom.fix a)
               else ins a
            end
      in
         Map.foldi ffn empty m
      end

   val fold = Map.foldi

   fun restrict (m, atoms) = Map.filteri (fn (a, _) => Atoms.mem (atoms, a)) m

   fun extend (m, k, s) = case Map.find (m, k) of
      NONE => Map.insert (m, k, s)
    | SOME _ => failwith "Ctx.extend: already present"

   fun extendl (m, l) = foldl (fn ((k, s), m) => extend (m, k, s)) m l

   fun remove (m, x) = fst (Map.removeExn (m, x))

   val find = Map.find

   val mem = Map.inDomain

   fun union (t1, t2) =
      let
         fun f (k1, k2) =
            if Sort.Base.eq (k1, k2) then k1
            else failwith "Ctx.union: unequal bindings"
      in
         Map.unionWith f (t1, t2)
      end

   val unionl = foldl union empty

   fun intersection (t1, t2) =
      let
         fun f (k1, k2) =
            if Sort.Base.eq (k1, k2) then k1
            else failwith "Ctx.intersection: unequal bindings"
      in
         Map.intersectWith f (t1, t2)
      end

   fun difference (t1, t2) =
      let
         val f = fn
            (SOME k, NONE) => SOME k
          | (NONE, _) => NONE
          | (SOME k1, SOME k2) =>
            if Sort.Base.eq (k1, k2) then NONE
            else failwith "Ctx.difference: unequal bindings"
      in
         Map.mergeWith f (t1, t2)
      end

   val isEmpty = Map.isEmpty

   fun pp m = Map.ppHoriz (fn (k, s) => %[Atom.pp k, $" : ", Sort.Base.pp s]) m

   fun atoms m =
      let
         fun f (k, _, (vs, ps)) = case k of
            Left x => (VSet.add (vs, x), ps)
          | Right a => (vs, PSet.add (ps, a))
      in
         Atoms.make (Map.foldli f (VSet.empty, PSet.empty) m)
      end

   val ( checkTerm :  t * Term.t * Sort.t -> unit
       , checkRel ) =
      let
         fun fail s = failwith ("Ctx.check: " ^ s)
         fun term (ctx, x, s) = case x of
            T.Var x =>
            let in
               case find (ctx, Left x) of
                  NONE => fail "unbound variable"
                | SOME s' => if Sort.Base.eq (s, s') then () else fail "sort mismatch"
            end
          | T.Param x =>
            let in
               case find (ctx, Right x) of
                  NONE => fail "unbound parameter"
                | SOME s' => if Sort.Base.eq (s, s') then () else fail "sort mismatch"
            end
          | T.Fn (f, ts) =>
            let
               val (ss, s') = S.func S.findExn f
            in
               if length ts <> length ss then failwith "Ctx.check: unequal lengths" else
               if Sort.Base.eq (s, s') then
                  List.app (fn (t, s) => term (ctx, t, s)) (List.zip (ts, ss))
               else fail "sort mismatch"
            end
         fun rel (ctx, r) =
            let
               val (p, ts) = Rel.dest r
               val ss = S.pred S.findExn p
            in
               List.app (fn (t, s) => term (ctx, t, s)) (List.zip (ts, ss))
            end
      in
         (term, rel)
      end

   val () = noWarnUnused (fn _ : printable => (pp, fix, restrict))
end
