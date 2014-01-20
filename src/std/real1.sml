
structure Real1 : Real = struct
   open Real
   type t = real
   val ofInt : int -> t = fromInt
end
