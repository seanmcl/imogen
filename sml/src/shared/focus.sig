
signature Focus = sig
   structure Left : sig
      datatype t =
         Neg of LFormula.neg
       | PosAtom of LFormula.pos
      val label : t -> Rel.t
      val pred : t -> Pred.t
   end

   structure Right : sig
      datatype t =
         Pos of LFormula.pos
       | NegAtom of LFormula.neg
       | Var
   end

   structure Seq : sig
      type t = Left.t list * Right.t
      include Printable where type printable = t
   end

   structure CSeq : sig
      datatype t = T of
         { seq : Seq.t
         , constr : CFormula.t
         , frozen : Func.set }
      include Printable where type printable = t
   end

   structure Rule : sig
      datatype t = T of
         { hyps : Seq.t list
         , concl : Seq.t
         , fresh : Param.set
         , constr : CFormula.t
         , proof : Fragment.t }
      include Printable where type printable = t
      val constr : t -> CFormula.t
   end

   structure Stable : sig
      datatype t = T of
         { formula : PFormula.neg
         , lformula : LFormula.neg
         , seqs: CSeq.t list
         , proof: Fragment.t
         , bipolarPreds : Pred.set
         , conflicts: Conflicts.t }
      include Printable where type printable = t
      val seqs : t -> CSeq.t list
   end

   structure Foci : sig
      datatype t = T of
         { rules: Rule.t list
         , goal: Seq.t
         , global: Left.t list
         , constr: CFormula.t }
      include Printable where type printable = t
   end

   val stabilize: PFormula.neg -> Stable.t
   (* set include_inconsistent_seq = true to add ⊥ | · ⊢ · *)
   val initial: {seq : CSeq.t, include_inconsistent_seq : bool} -> Foci.t
end
