
signature Active = sig
   type t

   val create : {global:Seq.Ants.t} -> t
   val size : t -> int

   (* pp (SOME n) db prints the n latest entries into db *)
   val pp : t -> int option -> PP.t

   val insert : t * Seq.t -> unit
   val subsumes : t * Seq.t -> bool
end
