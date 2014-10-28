
signature Seq = sig

   structure Cons : sig
      datatype t = P of Pred.t | Xi
      val isXi : t -> bool
      val eq : t * t -> bool
      structure Ops : sig
         datatype ops = datatype t
      end
   end
   type cons = Cons.t

   structure Ants : sig
      type t
      include Printable where type printable = t
      val empty : t
      val ofList: Pred.t list -> t
      val app: (Pred.t -> unit) -> t -> unit
      val fold: (Pred.t * 'a -> 'a) -> 'a -> t -> 'a
      val isEmpty: t -> bool
      val intersection: t * t -> t
      val difference: t * t -> t
      val union: t * t -> t
      val subset: t * t -> bool
      val mem: t * Pred.t -> bool
      val all : (Pred.t -> bool) -> t -> bool
   end
   type ants = Ants.t

   type t
   include Printable where type printable = t
   include Eqable where type eqable = t

   val ofFocus : Focus.Seq.t -> t
   val ppNoId: t -> PP.t
   val new: ants * cons -> t
   val preds: t -> Pred.set
   val id: t -> Id.t
   val ants: t -> ants
   val cons: t -> cons

   (* Union the antecedents and the consequent.  If the consequent has
      more than one formula, return NONE. *)
   val combine : t * t -> t option

   (* prio { seq, goal } gives the priority of a sequent with respect to a given
      goal.  A higher priority indicates a more important sequent. *)
   val prio: {seq : t, goal : t} -> int

   val subsumes: t * t -> bool
end
