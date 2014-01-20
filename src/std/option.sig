
signature Option = sig
   datatype 'a option = NONE | SOME of 'a
   val getOpt: 'a option * 'a -> 'a
   val isSome: 'a option -> bool
   val isNone: 'a option -> bool
   val valOf: 'a option -> 'a
   val app: ('a -> unit) -> 'a option -> unit
   val compose: ('a -> 'b) * ('c -> 'a option) -> 'c -> 'b option
   val composePartial: ('a -> 'b option) * ('c -> 'a option) -> 'c -> 'b option
   val filter: ('a -> bool) -> 'a -> 'a option
   val join: 'a option option -> 'a option
   val map: ('a -> 'b) -> 'a option -> 'b option
   val mapPartial: ('a -> 'b option) -> 'a option -> 'b option
   val option: 'a option -> 'b -> ('a -> 'b) -> 'b
   val value: 'a option -> 'a -> 'a
   val compare: ('a * 'a -> order) -> 'a option * 'a option -> order
   val extract: 'a option * exn -> 'a
end
