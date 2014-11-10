
signature Creatable = sig
   type creatable
   val next : unit -> creatable
   val reset : unit -> unit
end

signature Fixable = sig
   (* type atoms *)
   type fixable
   val fix : fixable -> fixable
   val unfix : fixable -> fixable
end

signature Fixable' = sig
   include Fixable
   type atoms
   val fix' : atoms -> fixable -> fixable
end

signature Paramable = sig
   type param
   type paramable
   val toParam : paramable -> param
   val ofParam : param -> paramable
end

signature Funcable = sig
   type func
   type funcable
   val toFunc : funcable -> func
   val ofFunc : func -> funcable
end

signature Predable = sig
   type pred
   type predable
   val toPred : predable -> pred
   val ofPred : pred -> predable
end

signature Var = sig
   type t
   include Collectable where type collectable = t
   include Comparable  where type comparable  = t
   include Creatable   where type creatable   = t
   include Fixable     where type fixable     = t
   include Hashable    where type hashable    = t
   include Parseable   where type parseable   = t
   include Printable   where type printable   = t
   include Showable    where type showable    = t
   include Paramable   where type paramable   = t
   val parse : Parse.Var.t -> t
   val isFixed : t -> bool
   val num : t -> int
end

signature Param = sig
   type t
   include Collectable where type collectable = t
   include Comparable  where type comparable  = t
   include Creatable   where type creatable   = t
   include Fixable     where type fixable     = t
   include Parseable   where type parseable   = t
   include Printable   where type printable   = t
   include Showable    where type showable    = t
   val parse : Parse.Param.t -> t
   val isFixed : t -> bool
   val invariant : set * {fresh:set} -> unit
   val num : t -> int
end

signature imogen.Atom = sig
   structure Var : Var
   structure Param : Param
   type t = (Var.t, Param.t) Either.t
   include Printable   where type printable   = t
   include Showable    where type showable    = t
   include Collectable where type collectable = t
   include Comparable  where type comparable  = t
   include Fixable     where type fixable     = t
end

signature Func = sig
   type t
   include Collectable where type collectable = t
   include Comparable  where type comparable  = t
   include Creatable   where type creatable   = t
   include Hashable    where type hashable    = t
   include Paramable   where type paramable   = t
   include Parseable   where type parseable   = t
   include Printable   where type printable   = t
   include Showable    where type showable    = t
   val parse : Parse.Func.t -> t
   type prec = int
   val maxPrec : int
   datatype assoc = Left | Right
   val fixity : t -> (prec * assoc) option

   structure Modal : sig
      val init : t
      val star : t
   end
   structure Linear : sig
      val eps : t
      val times : t
   end
   structure Ordered : sig
      val eps : t
      val times : t
      val inj : t
   end
end

signature Pred = sig
   type t
   include Collectable where type collectable = t
   include Comparable  where type comparable  = t
   include Creatable   where type creatable   = t
   include Hashable    where type hashable    = t
   include Parseable   where type parseable   = t
   include Printable   where type printable   = t
   include Showable    where type showable    = t
   include Funcable    where type funcable    = t
   datatype sign = Pos | Neg

   val parse : Parse.Pred.t -> t
   val sign : t -> sign
   val isInfix : t -> bool

   val pos : t -> bool
   val neg : t -> bool
   (* val isInfix : t -> bool *)

   val zero : t
   val one : t
   val top : t

   val ueq : t

   structure Modal : sig
      (* Simpson *)
      val le : t
      val atw : t
      val atf : t
      (* Davies *)
      val patom : t
      val natom : t
      val here : t
      val poss : t
   end

   structure Linear : sig
      val star : t (* ⊛ *)
      val atw : t (* ⓦ *)
      val atf : t (* ⓕ *)
   end

   structure Ordered : sig
      val star : t (* ⊛ *)
      val atw : t (* ⓦ *)
      val atf : t (* ⓕ *)
   end

   val classify : t -> Domain.t
   val inDomain : t -> bool
   val isConstr : t -> bool

end

signature Label = sig
   type t
   include Collectable where type collectable = t
   include Comparable  where type comparable  = t
   include Creatable   where type creatable   = t
   include Parseable   where type parseable   = t
   include Predable    where type predable    = t
   include Printable   where type printable   = t
   include Showable    where type showable    = t
   (* A label to insert in a proof term when there is clearly something
      wrong (e.g. it can't find a label.). *)
   val bug : t
end
