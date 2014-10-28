
signature RSeq = sig
   type t
   include Printable where type printable = t
   include Parseable where type parseable = t
   include Eqable where type eqable = t

   val make : Seq.t * Subst.t -> t
   val dest : t -> Seq.t * Subst.t
   val seq : t -> Seq.t
   val ofSeq : Seq.t -> t
   (* val subst : t -> Subst.t *)
   (* val apply : t * Subst.t -> t *)
   val subsumes : t * t * {global:Seq.Ants.t, atoms:Atoms.t} -> bool
   val subsumes' : t * t * {global:Seq.Ants.t, atoms:Atoms.t} -> Subst.t option
   (* val rename : t -> t *)
   val invariant : t * {global:Seq.Ants.t, atoms:Atoms.t, fresh:Param.set} -> unit
   (* val restrict : t * Atoms.t -> t *)
   val unfix : t -> t
   val atoms : t -> Atoms.t

   (* Union the antecedents and the consequent.  If the consequent has
      more than one formula, return NONE. *)
   val combine : t * t -> t option
end
