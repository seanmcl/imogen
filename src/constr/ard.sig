
signature ARD = sig
   type t

   val pp: t -> int option -> PP.t

   val create : { entails : Util.entails
                , global : CFormula.t } -> t
   val size : t -> int

   val insert: t * Rule.t -> unit
   val remove: t * RuleId.set -> unit

   val matches: t * Seq.t -> RuleId.set
   val find: t * RuleId.t -> Rule.t

   val subsumed: t * Seq.t -> RuleId.set
end
