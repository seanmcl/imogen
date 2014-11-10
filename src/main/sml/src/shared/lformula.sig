
(* Labelled polarized formulas.  This is not an idle abstraction.  We
   assign relations as labels for subformulas.  In the propositional
   case labeling is easy.  We can just maintain a map between the path
   to the subformula.  In the first order case it is more difficult.
   For instance, during a ∀-R rule, we invent a new parameter and
   substitute it throughout the formula.  A map will not work when a
   formula that generates parameters is inverted multiple times.  Consider

   ( ∃ X. q (X) ⇔ r ) ⇔ ( r ⇔ r )"

   The stable sequents are

     r, ↑(∃ X. ↓(q (X) o-o r)) ⊢ r
     r, ↑(∃ X. ↓(q (X) o-o r)) ⊢ r
     r o-o r ⊢ ∃ X. ↓(q (X) o-o r)

   Note that the same subformula will be inverted twice on the left,
   which will generate two distinct parameters.  If the path to that
   subformula is just looked up in the context, the second rule will
   have the wrong parameter name.  This module prevents such conflicts
   between generated parameters by storing the relational label with
   each subformula.  When a new parameter alpha is generated, the substitution
   {X ↦ alpha} is applied not only to the subformula, but to its label
   as well.
 *)
signature LFormula = sig

   type pos
   type neg

   datatype pos' =
      PAtom of Rel.t
    | Tensor of pos * pos
    | One
    | Sum of pos * pos
    | Zero
    | Down of neg
    | Ex of (Var.t * Sort.t) * pos

   and neg' =
      NAtom of Rel.t
    | With of neg * neg
    | Top
    | Lolli of pos * neg
    | BiLolli of neg * neg
    | Up of pos
    | All of (Var.t * Sort.t) * neg

   type ('a, 'b) t
   type 'a func = ('a, 'a) t
   type transform = (pos, neg) t

   val pos : ('a, 'b) t -> pos -> 'a
   val neg : ('a, 'b) t -> neg -> 'b

   val expose: (pos', neg') t

   val label: Rel.t func
   val pred: Pred.t func
   val preds: {pos:Pred.set, neg:Pred.set} func

   val make: PFormula.neg -> neg
   val pformula: (PFormula.pos, PFormula.neg) t
   val erase: imogen.Formula.t func

   (* Correctly staged. *)
   type unlabel = Rel.t -> (pos, neg) Either.t
   val unlabel: neg -> unlabel

   val pp: PP.t func
   val ppLabels: neg -> PP.t

   (* Substitution assumes all bound variables are distinct. *)
   val apply: Subst.t -> transform
   val apply1: Var.t * Term.t -> transform

   val freeze: transform
   (* val ctx: Ctx.t func *)
   val atoms: Atoms.t func

   val propositional : neg -> bool
end
