
signature Theory = sig
   type t

   val unify : Rel.t * Rel.t -> CSubst.t option
   val entails : t * t -> bool
end
