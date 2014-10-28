
signature Atoms = sig
   type t

   include Eqable where type eqable = t
   include Printable where type printable = t

   val empty : t

   val make : Var.set * Param.set -> t
   val vars : t -> Var.set
   val params : t -> Param.set
   val dest : t -> Var.set * Param.set
   val size : t -> int

   val add : t * Atom.t -> t
   val remove : t * Atom.t -> t
   val singleton : Atom.t -> t
   val mem : t * Atom.t -> bool
   val union : t * t -> t
   val unions : t list -> t
   val intersection : t * t -> t
   val difference : t * t -> t
   val disjoint : t * t -> bool
   val subset : t * t -> bool
   val isEmpty : t -> bool
   val fold : (Atom.t * 'a -> 'a) -> 'a -> t -> 'a
   val all : (Atom.t -> bool) -> t -> bool

   (* [fixed t] returns unfixed versions of the atoms that are fixed in t. *)
   val fixed : t -> t
   val fix : t * Param.set -> t
   val unfix : t -> t
end
