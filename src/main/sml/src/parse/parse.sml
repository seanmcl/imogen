
structure Parse1 : Parse = struct
   structure T = Types

   open General

   structure Prec = T.Prec
   structure Binop = T.Binop
   structure Unop = T.Unop
   structure Quant = T.Quant

   structure Var = struct
      open T.Var
      open imogen.Parser.Var
      type parseable = t
   end

   structure Param = struct
      open T.Param
      open imogen.Parser.Param
      type parseable = t
   end

   structure Func = struct
      open T.Func
      open imogen.Parser.Func
      type parseable = t
   end

   structure Term = struct
      open T.Term
      open imogen.Parser.Term
      type parseable = t
   end

   structure Pred = struct
      open T.Pred
      open imogen.Parser.Pred
      type parseable = t
   end

   structure Rel = struct
      open T.Rel
      open imogen.Parser.Rel
      type parseable = t
   end

   structure Sort = T.Sort
   structure Const = T.Const

   structure imogen.Formula = struct
      open T.imogen.Formula
      open imogen.Parser.imogen.Formula
      type parseable = t
   end

   structure Subst = struct
      open T.Subst
      open imogen.Parser.Subst
      type parseable = t
   end

   structure Ctx = T.Ctx

   structure Seq = struct
      open T.Seq
      open imogen.Parser.Seq
      type parseable = t
   end

   structure RSeq = struct
      open T.RSeq
      open imogen.Parser.RSeq
      type parseable = t
   end

   structure Rule = struct
      open T.Rule
      open imogen.Parser.Rule
      type parseable = t
   end

   structure Meta = Meta

   val () = noWarnUnused (fn _ :
      Func.parseable * Term.parseable * Pred.parseable
      * Rel.parseable * imogen.Formula.parseable * Subst.parseable
      * Seq.parseable * Rule.parseable * Sort.t * Ctx.t
      * Var.parseable * Param.parseable * RSeq.parseable
      => ())

end
