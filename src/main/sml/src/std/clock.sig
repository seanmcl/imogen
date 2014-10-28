
(* A simple counter *)
signature Clock = sig

   type t

   (* new startTime *)
   val new: int -> t
   val tick: t -> unit
   val time: t -> int
   val compare: t * t -> order
   val reset: t -> unit

end
