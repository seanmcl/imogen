
signature Proof = sig

   datatype parents = Parents of
      { rid : RuleId.t
      , parents : { id : Id.t, renaming : Subst.t } list
      , unifier : Subst.t }

   structure Node : sig
      datatype t =
         Initial of SC.t
       | Derived of parents
      val pp : t -> PP.t
   end

   type t

   val create : { f : LFormula.neg, global : Seq.Ants.t } -> t

   (* addSeq(t, q, (rid, [(qid1, θ1), ..., (qidN, θN)], θ))

      Let Rule.id (r) = rid.
      Let Rule.proof (r) = p_r.
      Let Seq.id (q_i) = qidi.
      Let p_i be the reconstructed proof of sequent q_i.
      Let θi rename p_i uniquely.  So dom(θi) = atoms(p_i) = atoms(q_i).
      and p_i[θi] are variable-disjoint proofs of q_i[θi].
      (We must rename the sequent/proof uniquely because the same sub-proof
       may be used in multiple places in a larger proof, and we must assure
       that variables are not captured.)

      So θ must have
      dom(θ) = ats(p_r) ∪ ats(p_1[θ1]) ∪ ... ∪ ats(p_N[θN])
      and img(θ) = ats(q).
      Then p_r(p_1[θ1], ..., p_N[θN])[θ] is an sc proof of q. *)
   val addSeq : t * Seq.t * Node.t -> unit
   val addRule : t * RuleInput.t -> unit

   val pp : t -> PP.t
   val parentRules : t * Id.t -> RuleId.set

   val reconstruct : t * Id.t -> SC.t

end
