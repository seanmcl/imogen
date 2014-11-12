
(* Δ = · | x : σ, Δ | a : σ, Δ  *)
signature Ctx = sig
   type t
   include Printable where type printable = t

   val empty : t
   val fold : (Atom.t * Sort.t * 'a -> 'a) -> 'a -> t -> 'a
   val extend : t * Atom.t * Sort.t -> t
   val extendl : t * (Atom.t * Sort.t) list -> t
   val find : t * Atom.t -> Sort.t option
   val mem : t * Atom.t -> bool
   val union : t * t -> t
   val unionl : t list -> t
   val intersection : t * t -> t
   val difference : t * t -> t
   val isEmpty : t -> bool
   val atoms : t -> Atoms.t
   val remove : t * Atom.t -> t
   val restrict : t * Atoms.t -> t
   val fix : t * Param.set -> t

   (* Σ, Δ ⊢ t : σ *)
   val checkTerm : t * Term.t * Sort.t -> unit

   (* Σ, Δ ⊢ r ok *)
   val checkRel : t * Rel.t -> unit
end
