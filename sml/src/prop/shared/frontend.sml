
structure Frontend : Frontend = struct
   open General
   type t = PFormula.neg
   type printable = t
   val pp = PFormula.neg PFormula.pp
   val parse = PFormula.parse
   val pformula = Fun.id
   val usesConstraints = false
   val () = noWarnUnused (fn _ : printable => ())
end
