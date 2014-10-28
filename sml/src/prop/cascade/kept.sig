
signature Kept = sig
   type t

   val create : unit -> t
   val isEmpty : t -> bool
   val size : t -> int
   val maxSize : t -> int
   val pp : t -> int option -> PP.t
   val insert : t * Seq.t * Proof.Node.t * int -> unit
   val remove : t * Id.set -> unit
   val next : t -> Seq.t * Proof.Node.t
   val subsumes : t * Seq.t -> bool
end
