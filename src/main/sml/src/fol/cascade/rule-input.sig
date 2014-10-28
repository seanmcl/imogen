
signature RuleInput = sig
   datatype t = T of
      { id : RuleId.t
      , hyp : Seq.t
      , hyps : Seq.t list
      , concl : Seq.t
      , fresh : Param.set
      , proof : Fragment.t }
   include Printable where type printable = t
   include Parseable where type parseable = t
   val ofFocus : Focus.Rule.t -> t

   val id : t -> RuleId.t
   val proof : t -> Fragment.t
   val atoms : t -> Atoms.t
   val rename : t * Subst.t -> t * Subst.t
end
