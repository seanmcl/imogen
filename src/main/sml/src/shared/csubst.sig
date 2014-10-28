
signature CSubst = sig
   type t
   include Printable where type printable = t

   val id : t
   val isId : t -> bool
   val dom : t -> Atoms.t
   val img : t -> Atoms.t
   val compose : t * t -> t
   val make : Subst.t * Term.eqs -> t
   val dest : t -> Subst.t * Term.eqs
   val subst : t -> Subst.t
   val eqs : t -> Term.eqs
   val restrict : t * Atoms.t -> t
   val apply : Term.t * t -> Term.t
   val applyR : Rel.t * t -> Rel.t
   (* Apply the substitution to the formula and conj the
      equalities *)
   val constr : t * CFormula.t -> CFormula.t
   val plus : t * t -> t option
   val plusl : t list -> t option
end
