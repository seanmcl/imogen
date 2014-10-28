
signature Subst = sig

   (* Invariants.
      1. t[id] = t
      2. t[θ1 o θ2] = t[θ1][θ2]
      3. t[θ][θ] = t[θ]
      4. x[θ] = x (for any fixed var x)
      5. a[θ] = a (for any fixed param a)
   *)
   type t

   include Printable where type printable = t
   include Eqable where type eqable = t
   include Parseable where type parseable = t
   include Fixable where type fixable = t

   val parse : Parse.Subst.t -> t

   type bind = (Var.t * Term.t, Param.t * Param.t) Either.t

   val all: (bind -> bool) -> t -> bool
   val fold: (bind * 'a -> 'a) -> 'a -> t -> 'a
   val ofList: bind list -> t
   val sing: bind -> t

   val id : t
   val isId : t -> bool

   val compose: t * t -> t
   val composes: t list -> t
   val union: t * t -> t
   val unions: t list -> t
   val fix': Atoms.t -> t -> t

   (* [extend] raises exns when
      - the occurs check fails
      - the lhs is fixed *)
   val extend: t * bind -> t
   val restrict: t * Atoms.t -> t
   val remove: t * (Var.t, Param.t) Either.t -> t

   val apply: Term.t * t -> Term.t
   val applyP: Param.t * t -> Param.t
   val applyV: Var.t * t -> Term.t
   val applyS: t * t -> t
   val applyB: bind * t -> bind
   val applyA: Atoms.t * t -> Atoms.t

   val dom: t -> Atoms.t
   val img: t -> Atoms.t

   val binding: t * Var.t -> Term.t option
   val bindingP: t * Param.t -> Param.t option

   (* does not rename fixed atoms. *)
   val renameTerm: Term.t * t -> Term.t * t
   (* val rename: t -> t *)
   val renameAtoms: Atoms.t -> t

   val toBindList : t -> bind list
   val toTermList : t -> (Term.t * Term.t) list

   val invariant : t -> unit
end
