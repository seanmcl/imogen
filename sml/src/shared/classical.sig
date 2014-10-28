
(*** Classical logic ***)

signature Classical = sig
   val simplify: Formula.t -> Formula.t

   (* Use the Glivenko-Orevkov-Avigad translation for classical formulas *)
   val doubleNegate: Formula.t -> Formula.t
end
