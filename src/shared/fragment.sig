
signature Fragment = sig
   datatype t =
      Leaf of SC.t
    | Node of SC.t -> t
   val pp: t -> PP.t
   val mapr: (SC.t -> SC.t) -> t -> t
   val mapr2: (SC.t * SC.t -> SC.t) -> t * t -> t
   val apply: t * Subst.t -> t
   val sc: t * SC.t list -> SC.t
   val atoms: t -> Atoms.t
end
