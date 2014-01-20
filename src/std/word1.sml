
structure Word1 : Word = struct
   open Word
   type t = word
   val ofInt = fromInt
end
