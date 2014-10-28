
signature OrdKey = sig
   type t
   val compare : t * t -> order
   val pp : t -> PP.t
end

