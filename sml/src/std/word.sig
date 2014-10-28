
signature Word = sig
   include WORD
   type t = word
   val ofInt : int -> t
end
