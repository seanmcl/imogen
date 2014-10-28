
(* Intuitionistic sequent calculus proof terms.
   We use these to construct natural deduction proofs. *)

signature SC = sig
   datatype t =
      Init of Rel.t
    | TensorR of t * t
    | TensorL of Rel.t * (Rel.t * Rel.t) * t
    | OneR
    | OneL of Rel.t * t
    | LolliR of Rel.t * t
    | LolliL of Rel.t * (Rel.t * t) * t
    | BiLolliR of (Rel.t * t) * (Rel.t * t)
    | BiLolliL1 of (Rel.t * t) * t * Rel.t
    | BiLolliL2 of (Rel.t * t) * t * Rel.t
    | WithR of t * t
    | WithL1 of Rel.t * (Rel.t * t)
    | WithL2 of Rel.t * (Rel.t * t)
    | PlusR1 of t
    | PlusR2 of t
    | PlusL of Rel.t * (Rel.t * t) * (Rel.t * t)
    | TopR
    | ZeroL of Rel.t
    | DownR of t
    | DownL of Rel.t * (Rel.t * t)
    | UpR of t
    | UpL of Rel.t * (Rel.t * t)
    | AllR of Param.t * t
    | AllL of Rel.t * (Rel.t * Term.t) * t
    | ExR of Term.t * t
    | ExL of Rel.t * (Rel.t * Param.t) * t
    | Hole

   include Printable where type printable = t

   (* Apply a substitution *)
   val apply: t * Subst.t -> t

   (* Convert a sequent calculus proof term to natural deduction proof term *)
   val nd: {norm : Rel.t -> Rel.t} -> t -> ND.t * (Label.t * Rel.t) list

   val unfix: t -> t

   val atoms: t -> Atoms.t

   val thaw: Func.set -> t -> t

   val reset : unit -> unit
end
