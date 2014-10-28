
functor OrdMapBaseFn (Key: OrdKey) =
   SplayMapFn (struct open Key type ord_key = t end)
(* RedBlackMapFn (struct open Key type ord_key = t end) *)

functor OrdMapFn (Key: OrdKey) : OrdMap where type key = Key.t = struct

   open PP.Ops
   type key = Key.t
   structure M = OrdMapBaseFn (Key)

   val toList = M.listItems
   val toListi = M.listItemsi

   structure L = List1

   infix 5 |-> |=>

   fun (k |-> v) m = M.insert (m, k, v)
   fun (k |=> v) = M.insert (M.empty, k, v)

   fun choose m =
      case M.firsti m of
         NONE => NONE
       | SOME (x, _) =>
         let
            val (m', y) = M.remove (m, x)
         in
            SOME (x, y, m')
         end

   fun difference (m1, m2) =
      M.filteri (fn (k,_) => not (isSome (M.find (m2, k)))) m1

   local
      exception NotEqual
   in
      fun equalBy p (m1, m2) =
         let
            (* Test that everything in m1 equals the corresponding entry in m2 *)
            fun appFn (k,v) =
               case M.find (m2, k) of
                  NONE => raise NotEqual
                | SOME v' => if p (v, v') then () else raise NotEqual
            (* Test that m2 doesn't have anything not in m1 *)
            fun appFn2 (k,_) =
               case M.find (m1, k) of
                  NONE => raise NotEqual
                | SOME _ => ()
         in
            M.appi appFn m1
          ; M.appi appFn2 m2
          ; true
         end
         handle NotEqual => false
   end

   fun equal ms = equalBy op= ms

   (* -------------------------------------------------------------------------- *)
   (*  Exists                                                                    *)
   (* -------------------------------------------------------------------------- *)

   fun search f (m:'a M.map) =
      let
         exception FoundIt of 'a
         fun appFn x = if f x then raise FoundIt x else ()
      in
         let in
            M.app appFn m
          ; NONE
         end
         handle FoundIt x => SOME x
      end

   fun searchi f (m:'a M.map) =
      let
         exception FoundIt of M.Key.ord_key * 'a
         fun appFn x = if f x then raise FoundIt x else ()
      in
         let in
            M.appi appFn m
          ; NONE
         end
         handle FoundIt x => SOME x
      end

   fun exists f (m:'a M.map) = isSome (search f m)
   fun existsi f (m:'a M.map) = isSome (searchi f m)

   fun all f = not o exists (not o f)
   fun alli f = not o existsi (not o f)

   fun ofList l = foldr M.insert' M.empty l

   fun ppVert f s = %[$"{",&(L.mapButlast (fn x => %[x, $", "], fn x => x) (map f (toListi s))),$"}"]
   fun ppHoriz f s = %[$"{",%(L.separate ($", ") (map f (toListi s))),$"}"]

   fun compare f (m1, m2) =
      let
         val items1 = toList m1
         val items2 = toList m2
      in
         Order.listOrder f (items1, items2)
      end

   val size = M.numItems

   open M
   type 'a t = 'a map
   val eqBy = equalBy
   val eq = equal
   val fold = foldl
   val foldi = foldli

   fun remove mx =
      SOME (M.remove mx)
      handle LibBase.NotFound => NONE

   val removeExn = M.remove

   fun findExn (m, k) = case M.find (m, k) of
      NONE => raise Fail "findExn"
    | SOME x => x

   val replace = insert
   fun insert (m, k, v) =
      case M.find (m, k) of
         NONE => replace (m, k, v)
       | SOME _ => raise Fail "Map.insert: key already exists"

   fun change f (m, k) = case f (M.find (m, k)) of
      SOME v => M.insert (m, k, v)
    | NONE =>
      case remove (m, k) of
         NONE => m
       | SOME (m, _) => m
end
