
signature OrdSet = sig
   type item
   type t

   val empty : t
   val singleton : item -> t
   val ofList : item list -> t
   val add  : t * item -> t
   val addList : t * item list -> t
   val remove : t * item -> t
   val removeExn : t * item -> t
   val mem : t * item -> bool
   val isEmpty : t -> bool
   val eq : (t * t) -> bool
   val compare : (t * t) -> order
   val subset : (t * t) -> bool
   val size : t -> int
   val toList : t -> item list
   val union : t * t -> t
   val intersection : t * t -> t
   val difference : t * t -> t
   val disjoint : t * t -> bool
   val map : (item -> item) -> t -> t
   val app : (item -> unit) -> t -> unit
   val fold : (item * 'b -> 'b) -> 'b -> t -> 'b
   val foldl : (item * 'b -> 'b) -> 'b -> t -> 'b
   val foldr : (item * 'b -> 'b) -> 'b -> t -> 'b
   val partition : (item -> bool) -> t -> (t * t)
   val filter : (item -> bool) -> t -> t
   val exists : (item -> bool) -> t -> bool
   val find : (item -> bool) -> t -> item option
   val findExn : (item -> bool) -> t -> item
   val ppItem: item -> PP.t
   val eqKey: item * item -> bool
   val choose: t -> (t * item) option
   val choose': t -> item
   val foldr1: (item * item -> item) -> t -> item
   val findRem: (item -> bool) -> t -> (item * t) option
   val unions: t list -> t
   val intersections: t list -> t
   val supset: t * t -> bool
   val allPairs: (item * item -> item) -> t * t -> t
   val mapPartial: (item -> item option) -> t -> t
   val all: (item -> bool) -> t -> bool
   val properSubset: t * t -> bool
   val findMap: (item -> 'a option) -> t -> 'a option
   val deleteIfMem: t * item -> t
   val pmap: (item -> 'a) -> t -> 'a list
   val pp: t -> PP.t
   val ppVert: t -> PP.t
end
