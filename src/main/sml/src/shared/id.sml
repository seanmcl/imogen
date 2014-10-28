
structure Id :> Id = struct
   type t = int
   type hashable = t
   type showable = t
   type collectable = t
   type comparable = t
   type printable = t

   local
      val next = ref 0
   in
      val reset = fn () => next := 0
      val next = fn () => !next before Ref.incr next
   end

   structure Table = Int.Table
   type 'a table = 'a Table.t
   structure HashSet = IntHashSet
   type hash_set = HashSet.t
   val hash = Word.fromInt
   val pp = PP.int
   val toString = Int.toString
   val compare = Int.compare
   val eq = op =
   val toInt = Fun.id
   structure S = CollectableFn
      (struct
         type t = t
         val pp = pp
         val compare = compare
      end)
   open S
   structure C = ComparableFn (type t = t val compare = compare)
   open C

   val hashSet = HashSet.fold (fn (x, s) => Set.add (s, x)) Set.empty
end

structure Id :> Id = Id
structure RuleId :> Id = Id
