
signature Entails = sig
   val f : Util.entails
   val simp : CFormula.simp
   val reduce : Term.t -> Term.t
   val unify : Term.t * Term.t -> CSubst.t option
end
