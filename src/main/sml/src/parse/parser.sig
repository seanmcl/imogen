
signature Parser = sig
   structure Var : sig
      val ofString : string -> Types.Var.t
   end

   structure Param : sig
      val ofString : string -> Types.Param.t
   end

   structure Func : sig
      val ofString : string -> Types.Func.t
   end

   structure Term : sig
      val ofString : string -> Types.Term.t
   end

   structure Pred : sig
      val ofString : string -> Types.Pred.t
   end

   structure Rel : sig
      val ofString : string -> Types.Rel.t
   end

   structure Formula : sig
      val ofString : string -> Types.Formula.t
      val ofFile : string -> Types.Formula.t
      val ofStdin : unit -> Types.Formula.t
   end

   structure Subst : sig
      val ofString : string -> Types.Subst.t
   end

   structure Seq : sig
      val ofString : string -> Types.Seq.t
   end

   structure RSeq : sig
      val ofString : string -> Types.RSeq.t
   end

   structure Rule : sig
      val ofString : string -> Types.Rule.t
   end
end
