
signature Seq = sig

   structure Cons : sig
      type t
      datatype t' = P of Rel.t | Xi
      val show : t -> t'
      val make : t' -> t
      val isXi : t -> bool
      val eq : t * t -> bool
      (* val unify : t * t -> (t * CSubst.t) option *)
      val atoms : t -> Atoms.t
      structure Ops : sig
         datatype ops = datatype t'
      end
   end

   structure Ants : sig
      type t
      include Printable where type printable = t
      val empty: t
      val ofList: Rel.t list -> t
      (* val mem: Rel.t * t -> bool *)
      val app: (Rel.t -> unit) -> t -> unit
      val filter: (Rel.t -> bool) -> t -> t
      val fold: (Rel.t * 'a -> 'a) -> 'a -> t -> 'a
      val isEmpty: t -> bool
      val intersection: t * t -> t
      val difference: t * t -> t
      val union: t * t -> t
      val hd: t -> Rel.t option
      (* val isSubset: t * t -> bool *)
      (* val all : (Rel.t -> bool) -> t -> bool *)
      (* val exists : (Rel.t -> bool) -> t -> bool *)
      val atoms : t -> Atoms.t
   end

   structure Pre : sig
      type t = Ants.t * Cons.t
      include Fixable' where type fixable = t
                         and type atoms = Atoms.t
      include Eqable where type eqable = t
      include Printable where type printable = t
      val ofFocus : Focus.Seq.t -> t
      val atoms : t -> Atoms.t
      val preds : t -> Pred.set
      val apply : t * Subst.t -> t
      val rename : t * Subst.t -> t * Subst.t

      (* fill (Γ ⊢ ·, p) = Γ ⊢ p
         fill (Γ ⊢ p, _) = Γ ⊢ p *)
      val fill: t * Cons.t -> t
      val contract : t * {global:Ants.t} -> (t * CSubst.t) list
   end

   type t
   include Printable where type printable = t
   include Eqable where type eqable = t
   include Parseable where type parseable = t

   val ofFocus : CFormula.t * Focus.Seq.t -> t
   val invariant : t * {global:Ants.t, fresh:Param.Set.t} -> unit
   val parse : Parse.Seq.t -> t
   val ppNoId: t -> PP.t
   val new: CFormula.t * Ants.t * Cons.t -> t
   val preds: t -> Pred.set
   val id: t -> Id.t
   val cons: t -> Cons.t
   val ants: t -> Ants.t
   val constr: t -> CFormula.t

   (* val ctx: t -> Ctx.t *)
   val atoms : t -> Atoms.t
   (* val params : t -> Param.set *)

   val rename: t * Subst.t -> t * Subst.t
   (* val apply: t * Subst.t -> t *)

   (* prio { seq, goal } gives the priority of a sequent with respect to a given
      goal.  A higher priority indicates a more important sequent. *)
   val prio: {seq : t, goal : t} -> int

   val subsumes :
      { subsumer : t
      , subsumed : t
      , global : CFormula.t
      , entails : Util.entails }
      -> Subst.t option

   (* match {seq, hyp, δ} returns a list of substitutions with the
      following properties
      - params (range θ|(vars hyp)) ∩ δ = ∅
        (fresh params don't show up in vars of hyp.)
      - if a,b ∈ δ then θ_p (a) ≠ θ_p (b)
        (fresh params are not unified.  This is trivially assured by fixing the
         fresh params before unification.)
      - either (cons seq)θ = (hyp seq)θ or one of those is empty.
      - For any mapping f : ants (seq) -> ants (hyp) where all pairs are
        simultaneously unifiable, there exists a θ in the list
        such that xθ = (f x)θ and no other pair are equated unless the
        equality is forced by the mapping f.
      match {seq, hyp, concl, fresh} assumes fresh are fixed in hyp and
      concl.
   *)
   val match
      :  { seq : t
         , hyp : Pre.t
         , concl : Pre.t
         , fresh : Param.set
         , global : Ants.t
         , atoms : Atoms.t}
      -> { concl : Pre.t
         , csubst : CSubst.t
         , filledHole : bool } list

   (* Contract with the global antecedents.  Don't contract any two relations
      with free variables.  Those are handled by matching and subsumption.
      This is purely an optimization to account for variable instantiations
      in the goal. *)
   val contract : t * {global:Ants.t} -> (t * CSubst.t) list

end
