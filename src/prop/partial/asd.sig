
signature ASD = sig
   type t

   val pp : t -> int option -> PP.t

   val create : unit -> t
   val size : t -> int

   val insert : t * Seq.t -> unit
   val remove : t * Id.set -> unit

   val subsumes : t * Seq.t -> bool
   val subsumed : t * Seq.t -> Id.set

   val matches : t * Rule.t -> Id.set
   val find : t * Id.t -> Seq.t
end
