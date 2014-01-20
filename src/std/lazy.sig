
signature Lazy = sig
   type 'a t
   exception Undefined
   val force: 'a t -> 'a
   val delay: (unit -> 'a) -> 'a t
   val undefined: 'a t
   val isUndefined: 'a t -> bool
   val inject : 'a -> 'a t
   val toString: ('a -> string) -> 'a t -> string
   val eq: ''a t * ''a t -> bool
   val eqBy: ('a * 'a -> bool) -> 'a t * 'a t -> bool
   val compare: ('a * 'a -> order) -> 'a t * 'a t -> order
   val map: ('a -> 'b) -> 'a t -> 'b t
   structure Ops: sig
      val ! : 'a t -> 'a
      val ? : 'a -> 'a t
      val % : (unit -> 'a) -> 'a t
   end
end
