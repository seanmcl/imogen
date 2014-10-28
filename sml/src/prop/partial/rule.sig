
signature Rule = sig
   type t
   include Printable where type printable = t

   val create: Seq.t list * Seq.t -> t

   val id: t -> RuleId.t
   val hyps: t -> Seq.t list
   val concl: t -> Seq.t
   val firstHyp: t -> Seq.t

   val prio: t * {goal : Seq.t} -> int

   structure Match : sig
      type rule = t
      datatype t =
         NewRules of rule list
       | NewSeqs of Seq.t list
       | NoMatch
      val pp: t -> PP.t
   end

   val ofFocus : Focus.Rule.t -> t
   val match: t * Seq.t -> Match.t
end
