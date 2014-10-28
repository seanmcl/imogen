
signature Logic = sig
   structure Axiom : sig
      datatype t = M | B | A4
   end

   datatype t = K | T | B | K4 | S4 | S5
   include Showable where type showable = t
   include Parseable where type parseable = t
   val axioms: t -> Axiom.t list
end
