
signature Rule = sig
   type id = RuleId.t

   structure Input : sig
      datatype t = T of
         { id : id
         , hyp : Seq.t
         , hyps : Seq.t list
         , concl : Seq.t
         , proof : Fragment.t }
      val pp : t -> PP.t
      val ofFocus : Focus.Rule.t -> t
   end

   type t

   (* give the priority of a sequent with respect to a given
      rule set.  A higher priority indicates a more important sequent. *)
   val prio : t * Seq.t -> int

   val create :
      { proof : Proof.t
      , conflicts : Conflicts.t
      , goal : Seq.t
      , initialSeqs : Seq.t list
      , preds : Pred.set
      , rules : Input.t list
      , kept : Kept.t
      , active : Active.t } -> t

   val size : t -> int
   val pp : t -> PP.t
   val removeSubsumed : t * Seq.t -> unit

   datatype out = Out of
      { seq : Seq.t
      , prio : int
      , parents : id * Id.t list }

   val match : t * Seq.t -> out list
end
