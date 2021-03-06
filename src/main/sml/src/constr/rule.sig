
signature Rule = sig
   type t
   include Printable where type printable = t

   val id: t -> RuleId.t
   val firstHyp: t -> Seq.Ants.t * Seq.Cons.t
   val hyps: t -> Seq.Pre.t list
   val concl: t -> Seq.Pre.t
   val constr: t -> CFormula.t
   val fresh: t -> Param.set
   val preds: t -> Pred.set

   val ofFocus : Focus.Rule.t -> t
   val rename: t -> t * Subst.t

   val prio: t * {goal : Seq.t} -> int

   structure Match : sig
      type rule = t
      datatype t =
         NewRules of (rule * Subst.t) list
       | NewSeqs of (Seq.t * Subst.t) list
       | NoMatch
      val pp: t -> PP.t
   end

   (* Reject rules for various reasons. *)
   val ok : t -> bool

   val match
      :  { seq : Seq.t
         , rule : t
         , global : Seq.Ants.t
         , simp : CFormula.simp }
      -> Match.t

   val contract: t -> (t * Subst.t) list
end

