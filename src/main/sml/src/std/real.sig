
signature Real = sig
   include REAL
   type t = real
   val ofInt : int -> t
end
