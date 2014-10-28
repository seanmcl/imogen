
structure Frontend : Frontend = struct
   type t = PFormula.neg
   type printable = t
   val pp = PFormula.neg PFormula.pp
   val parse = PFormula.parse
   val pformula = Fun.id
   val usesConstraints = false
end
