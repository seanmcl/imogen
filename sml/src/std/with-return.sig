
signature WithReturn = sig
   type ('a, 'b) return = 'a -> 'b
   val f : (('a, 'b) return -> 'a) -> 'a
end
