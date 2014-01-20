
signature Rule = sig
   type id = RuleId.t

   type t
   include Printable where type printable = t

   (* give the priority of a sequent with respect to a given
      rule set.  A higher priority indicates a more important sequent. *)
   val prio : t * Seq.t -> int

   val create :
      { proof : Proof.t
      , conflicts : Conflicts.t
      , goal : Seq.t
      , initialSeqs : Seq.t list
      , preds : Pred.set
      , rules : RuleInput.t list
      , kept : Kept.t
      , active : Active.t
      , global : Seq.Ants.t } -> t

   val size : t -> int
   val removeSubsumed : t * Seq.t -> unit

   datatype out = Out of
      { seq : Seq.t
      , prio : int
      , parents : Proof.parents }

   val match : t * Seq.t -> out list
end

