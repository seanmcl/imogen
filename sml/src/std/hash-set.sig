
signature HashSet = sig
   type item
   type t

   val create : int -> t
   val clear : t -> unit
   val ofList : item list -> t
   val singleton : item -> t
   val add : t * item -> unit
   val addList : t * item list -> unit

   (* Raise NotFound if not found. *)
   val delete : t * item -> unit
   val mem : t * item -> bool
   val isEmpty : t -> bool

   val size : t -> int
   val toList : t -> item list
   val app : (item -> unit) -> t -> unit
   val fold : (item * 'b -> 'b) -> 'b -> t -> 'b
   val exists : (item -> bool) -> t -> bool
   val pp: t -> PP.t
   val ppVert: t -> PP.t
end
