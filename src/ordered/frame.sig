
signature Frame = sig
   type t = Term.t * Term.t * Term.t
   val make : t -> Rel.t
   val dest : Rel.t -> t
   val extend : t * Term.t -> t
end
