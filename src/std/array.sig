
signature Array = sig
   include ARRAY
   type 'a t = 'a array
   val toList: 'a t -> 'a list
   val toString: ('a -> string) -> 'a t -> string
   val map: ('a -> 'b) -> 'a t -> 'b array
   val map2: ('a * 'b -> 'c) -> 'a t * 'b array -> 'c array
   val slice: 'a t * int * int option -> 'a t
   val all2: ('a * 'b -> bool) -> 'a t -> 'b array -> bool
   val qsort: ('a * 'a -> order) -> 'a t -> unit
end
