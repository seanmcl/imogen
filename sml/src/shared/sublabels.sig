
signature Sublabels = sig
   type t
   include Printable where type printable = t
   val lookup : t * Pred.t -> Pred.set
   val make : LFormula.neg -> t
end
