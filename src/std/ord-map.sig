
signature OrdMap = sig
   type key
   type 'a t
   val empty : 'a t
   val isEmpty : 'a t -> bool
   val singleton : (key * 'a) -> 'a t

   (* If the key exists, raise an exception. *)
   val insert  : 'a t * key * 'a -> 'a t
   (* If the key exists, discard the existing value. *)
   val replace  : 'a t * key * 'a -> 'a t
   val change : ('a option -> 'a option) -> 'a t * key -> 'a t
   val find : 'a t * key -> 'a option
   val findExn : 'a t * key -> 'a
   val lookup : 'a t * key -> 'a
   val inDomain : 'a t * key -> bool

   (* raises exn if key is missing *)
   val removeExn : 'a t * key -> 'a t * 'a
   val remove: 'a t * key -> ('a t * 'a) option

   val first : 'a t -> 'a option
   val firsti : 'a t -> (key * 'a) option
   val size : 'a t ->  int
   val toList  : 'a t -> 'a list
   val toListi : 'a t -> (key * 'a) list
   val listKeys : 'a t -> key list
   val collate : ('a * 'a -> order) -> ('a t * 'a t) -> order
   val unionWith  : ('a * 'a -> 'a) -> ('a t * 'a t) -> 'a t
   val unionWithi : (key * 'a * 'a -> 'a) -> ('a t * 'a t) -> 'a t
   val intersectWith  : ('a * 'b -> 'c) -> ('a t * 'b t) -> 'c t
   val intersectWithi
      :  (key * 'a * 'b -> 'c) -> ('a t * 'b t) -> 'c t
   val mergeWith
      :  ('a option * 'b option -> 'c option)
      -> ('a t * 'b t) -> 'c t
   val mergeWithi
      :  (key * 'a option * 'b option -> 'c option)
      -> ('a t * 'b t) -> 'c t
   val app  : ('a -> unit) -> 'a t -> unit
   val appi : ((key * 'a) -> unit) -> 'a t -> unit
   val map  : ('a -> 'b) -> 'a t -> 'b t
   val mapi : (key * 'a -> 'b) -> 'a t -> 'b t
   val fold  : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val foldi  : (key * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val foldl  : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val foldli : (key * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val foldr  : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val foldri : (key * 'a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val filter  : ('a -> bool) -> 'a t -> 'a t
   val filteri : (key * 'a -> bool) -> 'a t -> 'a t
   val mapPartial  : ('a -> 'b option) -> 'a t -> 'b t
   val mapPartiali : (key * 'a -> 'b option) -> 'a t -> 'b t
   val |-> : key * 'a -> 'a t -> 'a t
   val |=> : key * 'a -> 'a t
   val difference: 'a t * 'a t -> 'a t
   val eqBy: ('a * 'a -> bool) -> 'a t * 'a t -> bool
   val eq: ''a t * ''a t -> bool
   val choose: 'a t -> (key * 'a * 'a t) option
   val search: ('a -> bool) -> 'a t -> 'a option
   val searchi: (key * 'a -> bool) -> 'a t -> (key * 'a) option
   val exists: ('a -> bool) -> 'a t -> bool
   val existsi: (key * 'a -> bool) -> 'a t -> bool
   val all: ('a -> bool) -> 'a t -> bool
   val alli: (key * 'a -> bool) -> 'a t -> bool
   val ofList: (key * 'a) list -> 'a t
   val ppHoriz: (key * 'a -> PP.t) -> 'a t -> PP.t
   val ppVert:  (key * 'a -> PP.t) -> 'a t -> PP.t
   val compare: ('a * 'a -> order) -> 'a t * 'a t -> order
end
