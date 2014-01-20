
signature Conflicts = sig
   type t
   include Printable where type printable = t

   val make : Sublabels.t * LFormula.neg -> t
   val allowed : t * Pred.set -> bool
   val dummy : t
   val restrict : t * Pred.set -> t
end
