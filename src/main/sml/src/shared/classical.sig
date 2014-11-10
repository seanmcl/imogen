
(*** Classical logic ***)

signature Classical = sig
   val simplify: imogen.Formula.t -> imogen.Formula.t

   (* Use the Glivenko-Orevkov-Avigad translation for classical formulas *)
   val doubleNegate: imogen.Formula.t -> imogen.Formula.t
end
