
signature CUnif = sig
   val unify: Rel.t * Rel.t -> CSubst.t option
   val eq : Term.t * Term.t -> bool
   val reduce : Rel.t -> Rel.t

   val instance: Rel.t * Rel.t -> bool
   val general: Rel.t * Rel.t -> bool
   val variant: Rel.t * Rel.t -> bool
   val unifiable: Rel.t * Rel.t -> bool
end
