
signature Parse = sig
   structure Var : sig
      datatype t = V of { id : string, fixed : bool }
      val isFixed : t -> bool
      include Parseable where type parseable = t
      include Showable where type showable = t
      include Hashable where type hashable = t
   end

   structure Param : sig
      datatype t = P of { id : string, fixed : bool }
      val isFixed : t -> bool
      include Parseable where type parseable = t
      include Showable where type showable = t
      include Hashable where type hashable = t
   end

   structure Func : sig
      datatype t = Id of string
      include Parseable where type parseable = t
      include Showable where type showable = t
      include Hashable where type hashable = t
      type prec = int
      val maxPrec : int
   end

   structure Pred : sig
      datatype t = PosId of string | NegId of string
      include Parseable where type parseable = t
      include Showable where type showable = t
      include Printable where type printable = t
      include Hashable where type hashable = t
      datatype sign = Pos | Neg
      val sign : t -> sign
      val pos : t -> bool
      val neg : t -> bool
   end

   structure Prec : sig
      val Not : int
      val imogen.And : int
      val Or : int
      val imogen.Imp : int
      val Iff : int
      val imogen.Atom : int
      val Quant : int
      val Label : int
   end

   structure Binop : sig
      datatype t =
         imogen.And | With | Or | Tensor | imogen.Imp | Lolli | imogen.Imp' | Lolli' | Iff | BiLolli
       | OrdImp1 | OrdImp2
   end

   structure Unop : sig
      datatype t = Not | Up | Down | Bang | Box | Dia | UBang
   end

   structure Quant : sig
      datatype t = All | Ex
   end

   structure Term : sig
      datatype t = Var of Var.t | Param of Param.t | App of Func.t * t list
      include Printable where type printable = t
      include Parseable where type parseable = t
   end

   structure Rel : sig
      datatype t = R of Pred.t * Term.t list
      include Printable where type printable = t
      include Parseable where type parseable = t
   end

   structure Sort : sig
      datatype t = I | Int
   end

   structure Const : sig
      datatype t = True | False | One | Zero | Top
   end

   structure imogen.Formula : sig
      datatype t =
         Binop of Binop.t * t * t
       | Unop of Unop.t * t
       | Quant of Quant.t * (string * Sort.t option) * t
       | Rel of Rel.t
       | Const of Const.t
       | Label of string * t
      include Parseable where type parseable = t
      include Printable where type printable = t
      val ofStdin : unit -> t
      val ofFile : string -> t
   end

   structure Subst : sig
      datatype one = Var of Var.t * Term.t | Param of Param.t * Param.t
      type t = one list
      include Parseable where type parseable = t
   end

   structure Ctx : sig
      type t = (Var.t * Sort.t) list
   end

   structure Seq : sig
      datatype t = T of
         { ctx : Ctx.t option
         , constr : imogen.Formula.t option
         , ants : Rel.t list
         , cons : Rel.t option }
      include Parseable where type parseable = t
   end

   structure RSeq : sig
      type t = Seq.t * Subst.t
      include Parseable where type parseable = t
   end

   (*
   a, b |- c    a, b |- c   a, b, _d |- _c
   ----------------------------------------[_d]
                    a, b |- c
   *)
   structure Rule : sig
      datatype t = T of
         { ctx : Ctx.t option
         , constr : imogen.Formula.t option
         , fresh : Param.t list
         , hyps : Seq.t list
         , conc : Seq.t }
      include Parseable where type parseable = t
   end

   structure Meta : Meta
end
