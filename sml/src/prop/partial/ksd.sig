
signature KSD = sig
   type t

   val create : unit -> t
   val isEmpty : t -> bool
   val size : t -> int
   val pp : t -> int option -> PP.t
   val insert : t * Seq.t * PDB.Node.t * int -> unit
   val remove : t * Id.set -> unit
   val next : t -> Seq.t * PDB.Node.t
   val subsumes : t * Seq.t -> bool
   val subsumed : t * Seq.t -> Id.set
end
