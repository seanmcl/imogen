
functor ComparableFn (K : ComparableKey) : Comparable = struct
   open K
   type comparable = t
   fun eq (x, y) = compare (x, y) = EQUAL
   fun lt (x, y) = compare (x, y) = LESS
   fun gt (x, y) = compare (x, y) = GREATER
   fun le (x, y) = compare (x, y) <> GREATER
   fun ge (x, y) = compare (x, y) <> LESS
end

functor CollectableFn (K : CollectableKey) : Collectable = struct
   open K
   type collectable = t

   structure Set =
      OrdSetFn
      (struct
         type t = t
         val compare = compare
         val pp = pp
      end)

   type set = Set.t

   structure Map =
      OrdMapFn
      (struct
         type t = t
         val compare = compare
         val pp = pp
      end)

   type 'a map = 'a Map.t
end

functor HashableFn (K : HashKey) : Hashable = struct
   open K
   type hashable = t
   structure Table = HashTableFn(K)
   type 'a table = 'a Table.t
   structure HashSet = HashSetFn(K)
   type hash_set = HashSet.t
end
