
(* Unification.

   Variables and parameters can be fixed or unfixed.

   Unfixed variables unify with anything.
   A fixed variable is treated as a constant and unifies
   only with itself.

   Unfixed parameters unify with any other parameter.
   A fixed parameter is treated as a constant and unifies
   only with itself. *)

signature Unif = sig
   val unify: Term.eqs -> Subst.t option
   val unify1: Term.eq -> Subst.t option

   (* {x1 -> t1, ...} + {y1 -> s1,...} = unify [(x1, t1), ..., (y1, s1), ...] *)
   val plus: Subst.t * Subst.t -> Subst.t option
   val plusl: Subst.t list -> Subst.t option

   (* θ1 ≤ θ2 if there exists θ such that θ1 · θ = θ2.  For bindings
      x -> t that are in both θ1 and θ2, this is easily determined by
      adding both equalities and calling unify.  If x -> t is in θ1 but
      not θ2, we must have that t = x' for some variable x' and we add
      the equality x -> fix(x) since θ2(x) = x.  *)
   val subsumes': Subst.t * Subst.t * Atoms.t -> Subst.t option
   val subsumes: Subst.t * Subst.t * Atoms.t -> bool

   (*** Util ***)

   val instance: Term.t * Term.t -> bool
   val general: Term.t * Term.t -> bool
   val variant: Term.t * Term.t -> bool
   val unifiable: Term.t * Term.t -> bool

   (*** Unification ***)

   structure Rel : sig
      val unify: Rel.t * Rel.t -> Subst.t option
      (* val unifys: Rel.t list -> Subst.t option *)
   end

end
