
signature World = sig
   type t
   val ofTerm : Term.t -> t
   val toTerm : t -> Term.t
   val getVar : t -> Var.t option
   val isEps : t -> bool
   val hasConst : t -> bool
   val apply : t * Subst.t -> t
   val numVars : t -> int
   val reduce : Term.t -> Term.t
   val eps : t
   val epsT : Term.t
   (* val times : t * t -> t *)
   val timesT : Term.t * Term.t -> Term.t
   val all : (Term.t -> bool) -> t -> bool
   val exists : (Term.t -> bool) -> t -> bool
   val findRem : (Term.t -> bool) -> t -> (Term.t * t) option
   val removeExn : t * Term.t -> t
   val mem : t * Term.t -> bool
   val partition : (Term.t -> bool) -> t -> t * t
   val size : t -> int
   val add : t * Term.t -> t
   val toList : t -> Term.t list
   val map : (Term.t -> Term.t) -> t -> t
   (* val ofList : Term.t list -> t *)

   structure Eqs : sig
      type w = t
      type t = (w * w) list
      val ofForm : CFormula.t -> t
      val toForm : t -> CFormula.t
      val toEqs : t -> Term.eqs
      val ofEqs : Term.eqs -> t
      val apply : t * Subst.t -> t
   end
end
