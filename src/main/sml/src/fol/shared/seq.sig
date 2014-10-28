
signature Seq = sig

   structure Cons : sig
      datatype t = P of Rel.t | Xi
      val isXi : t -> bool
      val eq : t * t -> bool
      val unify : t * t -> (t * Subst.t) option
      val atoms : t -> Atoms.t
      structure Ops : sig
         datatype ops = datatype t
      end
   end

   structure Ants : sig
      type t
      include Printable where type printable = t
      val empty: t
      val ofList: Rel.t list -> t
      val mem: Rel.t * t -> bool
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

   type t
   include Printable where type printable = t
   include Eqable where type eqable = t
   include Parseable where type parseable = t
   include Fixable' where type fixable = t
                      and type atoms = Atoms.t

   val ofFocus : Focus.Seq.t -> t
   val invariant : t * {global:Ants.t, fresh:Param.Set.t} -> unit
   val parse : Parse.Seq.t -> t
   val ppNoId: t -> PP.t
   val new: Ants.t * Cons.t -> t
   val preds: t -> Pred.set
   val id: t -> Id.t
   val ants: t -> Ants.t
   val cons: t -> Cons.t

   (* fill (Γ ⊢ ·, p) = Γ ⊢ p
      fill (Γ ⊢ p, _) = Γ ⊢ p *)
   val fill: t * Cons.t -> t

   (* val ctx: t -> Ctx.t *)
   val atoms : t -> Atoms.t
   val params : t -> Param.set

   val rename: t * Subst.t -> t * Subst.t
   val apply: t * Subst.t -> t

   (* prio { seq, goal } gives the priority of a sequent with respect to a given
      goal.  A higher priority indicates a more important sequent. *)
   val prio: {seq : t, goal : t} -> int

   (* if global = SOME Γ, allow antecedents in the subsuming sequent to
      contract with Γ. *)
   val subsumes' : t * t * {global:Ants.t option} -> Subst.t option
   val subsumes : t * t * {global:Ants.t option} -> bool

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
      :  { seq:t
         , hyp:t
         , concl:t
         , fresh:Param.set
         , global:Ants.t
         , atoms:Atoms.t}
      -> { concl : t
         , theta : Subst.t
         , filledHole : bool } list

   (* Contract with the global antecedents.  Don't contract any two relations
      with free variables.  Those are handled by matching and subsumption.
      This is purely an optimization to account for variable instantiations
      in the goal. *)
   val contract : t * {global:Ants.t} -> (t * Subst.t) list

end
