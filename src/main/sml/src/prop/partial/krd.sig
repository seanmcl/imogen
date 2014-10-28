
signature KRD = sig
   type t

   val pp: t -> int option -> PP.t

   val create: unit -> t
   val size: t -> int
   val isEmpty: t -> bool

   val insert: t * Rule.t * PDB.Node.t * {prio:int} -> unit
   val remove: t * RuleId.set -> unit
   val next: t -> Rule.t * PDB.Node.t

   val subsumed: t * Seq.t -> RuleId.set
end
