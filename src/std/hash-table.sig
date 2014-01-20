
signature HashKey = sig
   type t
   val hash : t -> word
   val eq : t * t -> bool
   val pp : t -> PP.t
end

signature HashTable = sig
   type ('a, 'b) t

   val create:  (('a -> word) * ('a * 'a -> bool)) -> (int * exn) -> ('a,'b) t
   val clear : ('a, 'b) t -> unit

   (* Existing items are discarded.*)
   val replace : ('a, 'b) t -> ('a * 'b) -> unit

   (* If an item exists, raise an exceptionl *)
   val insertExn : ('a, 'b) t -> ('a * 'b) -> unit

   val inDomain : ('a, 'b) t -> 'a -> bool
   val findExn : ('a, 'b) t -> 'a -> 'b
   val find : ('a, 'b) t -> 'a -> 'b option
   val remove : ('a, 'b) t -> 'a -> 'b
   val size : ('a, 'b) t ->  int
   val app  : ('b -> unit) -> ('a, 'b) t -> unit
   val appi : (('a * 'b) -> unit) -> ('a, 'b) t -> unit
   val map  : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
   val mapi : (('a * 'b) -> 'c) -> ('a, 'b) t -> ('a, 'c) t
   val fold  : (('b *'c) -> 'c) -> 'c -> ('a, 'b) t -> 'c
   val foldi : (('a * 'b * 'c) -> 'c) -> 'c -> ('a, 'b) t -> 'c
   val modify  : ('b -> 'b) -> ('a, 'b) t -> unit
   val modifyi : (('a * 'b) -> 'b) -> ('a, 'b) t -> unit
   val filter  : ('b -> bool) -> ('a, 'b) t -> unit
   val filteri : (('a * 'b) -> bool) -> ('a, 'b) t -> unit
   val copy : ('a, 'b) t -> ('a, 'b) t
   val bucketSizes : ('a, 'b) t -> int list
   val toList : ('a, 'b) t -> 'b list
   val toListi : ('a, 'b) t -> ('a * 'b) list
   val all: ('a * 'b -> bool) -> ('a, 'b) t -> bool
   val pp: ('a * 'b -> PP.t) -> ('a, 'b) t -> PP.t
end

signature MonoHashTable = sig
   type 'a t
   type key
   val create : (int * exn) -> 'a t
   val clear : 'a t -> unit
   val insertExn : 'a t -> (key * 'a) -> unit
   val replace : 'a t -> (key * 'a) -> unit
   val inDomain : 'a t -> key -> bool
   val findExn : 'a t -> key -> 'a
   val find : 'a t -> key -> 'a option
   val remove : 'a t -> key -> 'a
   val size : 'a t ->  int
   val app  : ('a -> unit) -> 'a t -> unit
   val appi : ((key * 'a) -> unit) -> 'a t -> unit
   val map  : ('a -> 'b) -> 'a t -> 'b t
   val mapi : ((key * 'a) -> 'b) -> 'a t -> 'b t
   val fold  : (('a * 'b) -> 'b) -> 'b -> 'a t -> 'b
   val foldi : ((key * 'a * 'b) -> 'b) -> 'b -> 'a t -> 'b
   val modify  : ('a -> 'a) -> 'a t -> unit
   val modifyi : ((key * 'a) -> 'a) -> 'a t -> unit
   val filter  : ('a -> bool) -> 'a t -> unit
   val filteri : ((key * 'a) -> bool) -> 'a t -> unit
   val copy : 'a t -> 'a t
   val bucketSizes : 'a t -> int list
   val toList : 'a t -> 'a list
   val toListi : 'a t -> (key * 'a) list
   val update : 'a t -> ('a option -> 'a) -> key -> unit
   val all: (key * 'a -> bool) -> 'a t -> bool
   val pp: (key * 'a -> PP.t) -> 'a t -> PP.t
end
