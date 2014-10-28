
functor HashSetFn (Key : HashKey) :> HashSet
   where type item = Key.t = struct
   structure L = List1
   structure Key = Key
   structure Table = HashTableFn (Key)

   open PP.Ops

   type t = unit Table.t
   type item = Key.t
   exception NotFound
   fun create n = Table.create (n ,NotFound)
   val clear = Table.clear
   fun add (t, x) =
      if Table.inDomain t x then () else Table.insertExn t (x, ())
   fun addList (t, l) = List.app (fn x => add (t, x)) l
   fun delete (t, x) = Table.remove t x
   fun mem (t, x) = isSome (Table.find t x)
   val size = Table.size
   fun isEmpty t = size t = 0
   fun toList t = Table.foldi (fn (x, _, xs) => x :: xs) [] t
   fun app f t = Table.appi (fn (x, _) => f x) t
   fun fold f init t = Table.foldi (fn (x, _, acc) => f (x, acc)) init t
   fun ofList l =
      let
         val n = length l
         val t = create n
      in
         List.app (fn x => add (t, x)) l
       ; t
      end
   fun exists f t =
      WithReturn.f
         (fn return =>
            let in
               Table.appi (fn (x, _) => if f x then return true else ()) t
             ; false
            end)

   fun singleton x = ofList [x]
   fun ppVert s = %[$"{",&(L.mapButlast (fn x => %[x, $","], fn x => x)
                              (map Key.pp (toList s))),$"}"]

   fun pp s = %[$"{",%(L.separate ($", ") (map Key.pp (toList s))),$"}"]
end
