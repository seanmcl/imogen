
signature Eqable = sig
   type eqable
   val eq : eqable * eqable -> bool
end

signature Parseable = sig
   type parseable
   val ofString : string -> parseable
end

signature Showable = sig
   type showable
   val toString : showable -> string
end

signature Printable = sig
   type printable
   val pp : printable -> PP.t
end

signature Printable1 = sig
   type 'a printable1
   val pp : ('a -> PP.t) -> 'a printable1 -> PP.t
end

signature ComparableKey = sig
   type t
   val compare : t * t -> order
end

signature Comparable = sig
   type comparable
   val compare : comparable * comparable -> order
   val eq : comparable * comparable -> bool
   val lt : comparable * comparable -> bool
   val le : comparable * comparable -> bool
   val gt : comparable * comparable -> bool
   val ge : comparable * comparable -> bool
end

signature Hashable = sig
   type hashable
   val hash : hashable -> Word.word
   structure Table : MonoHashTable where type key = hashable
   structure HashSet : HashSet where type item = hashable
   type 'a table = 'a Table.t
   type hash_set = HashSet.t
end

signature CollectableKey = sig
   type t
   val compare : t * t -> order
   val pp : t -> PP.t
end

signature Collectable = sig
   type collectable
   structure Set : OrdSet where type item = collectable
   type set = Set.t
   structure Map : OrdMap where type key = collectable
   type 'a map = 'a Map.t
end

signature Intable = sig
   type intable
   val ofInt : int -> intable
   val toInt : intable -> int
end
