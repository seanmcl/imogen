
(*** Relations ***)

signature Rel = sig
   type t

   include Collectable where type collectable = t
   include Comparable where type comparable = t
   include Showable where type showable = t
   include Printable where type printable = t
   include Parseable where type parseable = t
   include Fixable' where type fixable = t
                      and type atoms = Atoms.t

   val zero : t
   val one : t
   val top : t

   val eq' : (Term.t * Term.t -> bool) -> t * t -> bool

   val parse : Parse.Rel.t -> t

   val dest: t -> Pred.t * Term.t list
   val make: Pred.t * Term.t list -> t
   val pred: t -> Pred.t
   val sign: t -> Pred.sign
   val isConstr: t -> bool

   val funcs: t -> Func.set

   (*** Apply a substitution to a relation. ***)
   val apply: t * Subst.t -> t
   val apply1: t * (Var.t * Term.t) -> t
   val applySet: set * Subst.t -> set

   (*
    Generate new variable and parameter names and
    return the resulting substitution.
    *)
   val rename: t * Subst.t -> t * Subst.t
   val paramSubst: t * (Param.t * Var.t) -> t

   val atoms : t -> Atoms.t

   (*** Freeze and thaw the global parameters introduced
    in the first inversion phase ***)

   val freeze: t -> t
   val thaw: Func.set -> t -> t

   (* Convert to and from a term, for storing in a term index
      P (t1, ..., tn) --> f_P (t1, ..., tn) *)
   val toTerm: t -> Term.t
end
