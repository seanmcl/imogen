
signature CFormula = sig
   structure Export : sig
      datatype t =
         imogen.Atom of Rel.t
       | Top
       | Bot
       | imogen.And of t * t
       | imogen.Imp of Rel.t * t
       | All of (Var.t * Sort.t) * t
       | Ex of (Var.t * Sort.t) * t
       | Hole
   end
   datatype t = datatype Export.t
   type simp = t -> Subst.t * t
   val noSimp : simp

   include Printable where type printable = t
   include Eqable where type eqable = t
   include Fixable where type fixable = t

   (* Free variables and parameters. *)
   val atoms : t -> Atoms.t
   val isBot : t -> bool

   (* Capture avoiding substitution *)
   val apply1 : t * (Var.t * Term.t) -> t
   val apply : t * Subst.t -> t
   (* val rename : t * Subst.t -> t * Subst.t *)
   val propositional : t -> bool
   val simplify : t -> t
   val listConj : t list -> t
   val conjuncts : t -> t list
   val paramSubst : t * (Param.t * Var.t) -> t
   val quantifyParams : t * Param.set -> t
   val quantifyVars : t * Var.set -> t
   val fill : {fillee : t, filler : t} -> t
   val fillHolesWithTop : t -> t
   val freeze: t -> t
   val thaw: Func.set -> t -> t
end
