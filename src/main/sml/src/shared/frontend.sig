
signature Frontend = sig
   type t
   include Printable where type printable = t

   val parse : Parse.Formula.t -> t
   val pformula : t -> PFormula.neg
   val usesConstraints : bool
end
