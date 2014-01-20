
functor OrdSetBaseFn (Key : OrdKey) =
   SplaySetFn (struct open Key type ord_key = t end)
(* RedBlackSetFn (struct open Key type ord_key = t end) *)

functor OrdSetFn (Key: OrdKey) : OrdSet where type item = Key.t = struct
   structure L = List1
   open PP.Ops
   infixr 0 `

   structure Set = OrdSetBaseFn (Key)

   val toList = Set.listItems

   fun eqKey k12 = Set.Key.compare k12 = EQUAL

   fun choose s =
      case Set.find (Fun.const true) s of
         NONE => NONE
       | SOME x => SOME (Set.delete (s,x),x)

   fun choose' s =
      case Set.find (Fun.const true) s of
         NONE => raise Empty
       | SOME x => x

   val mem = Set.member

   fun deleteIfMem (sx as (s,_)) =
      if mem sx then Set.delete sx else s

   fun findMap p =
      let
         exception FoundIt of 'a
         fun p' x = case p x of
            NONE => false
          | SOME y => raise FoundIt y
      in
         fn s => (ignore (Set.find p' s); NONE)
            handle FoundIt x => SOME x
      end

   fun findRem p s = case Set.find p s of
      NONE => NONE
    | SOME x => SOME (x,Set.delete (s,x))

   fun findExn p s = case Set.find p s of
      NONE => raise Fail "findExn"
    | SOME x => x

   fun unions l = L.foldl Set.union Set.empty l

   fun supset (s, t) = Set.isSubset (t, s)

   fun intersections [] = Set.empty
     | intersections (h::t) = L.foldl Set.intersection h t

   fun allPairs f (xs, ys) =
      let
         fun mapFn x = Set.map (fn y => f (x, y)) ys
      in
         Set.foldr (fn (x, acc) => Set.union (acc, mapFn x)) Set.empty xs
      end

   fun mapPartial f s =
      Set.foldr (fn (x, acc) => case f x of SOME y => Set.add (acc, y) | NONE => acc) Set.empty s

   (* NJ doesn't have this *)
   fun ofList l = foldl Set.add' Set.empty l

   (* avoid MLton warning *)
   val () = ignore (ofList)

   fun all f xs = not (Set.exists (not o f) xs)

   fun pmap f xs = Set.foldr (fn (x, acc) => f x::acc) [] xs

   fun ppVert s = %[ $"{",
                    &(L.mapButlast (fn x => %[x, $","], fn x => x)
                         (map Key.pp (toList s))), $"}"]

   fun ppSet s = %[$"{",%(L.separate ($", ") (map Key.pp (toList s))),$"}"]

   fun properSubset (s, s') = Set.isSubset (s, s') andalso not (Set.equal (s, s'))

   fun foldr1 f s = case choose s of
      NONE => raise Fail "Impossible"
    | SOME (s, e) => Set.foldr f e s

   val ppItem = Key.pp
   val size = Set.numItems

   open Set

   val disjoint = isEmpty o intersection

   type t = set
   val pp = ppSet
   val eq = equal
   val fold = foldl
   val subset = isSubset
   val removeExn = delete
   fun remove (t, x) = removeExn (t, x) handle _ => t
end
