
(*
 Equational theory

 ε ⋆ p ≡ p
 p ⋆ q ≡ q ⋆ p
 p ⋆ (q ⋆ r) ≡ (p ⋆ q) ⋆ r

variable matching.  This unification problem is unitary.
Then we solve the AC1 world unification problem of the pi, qi.
Since pi,qi can have constants (the parameters are effectively
constants in this setting), we no longer have unique mgus.  For
example, c ≡ α1 ⋆ α2 has 2 solutions, { α1 ↦ c, α2 ↦ ε }
and { α1 ↦ ε, α2 ↦ c }.  A full solution involves translating
the problem into nonhomogenous diophantine equations and solving
those.  The problem is NP-complete, and a bit of a mess.

Instead, we will try to find an i such that the problem
pi ⋆ qi ≡ pi' ⋆ qi' is unitary.  Then we can apply the
substitution to the rest of the equations and hope that
the substitution generates more unitary equations.  If
not, we will maintain the non-unitary equations as constraints.

0) remove all ε from Δ
 p ⋆ ε ↦ p

1) if α ≡ p ∈ Δ or p ≡ α ∈ Δ
 Δ ↦ {α ↦ p} (Δ \ {α ≡ p})
 Add {α ↦ p} to θ

2) If p ≡ ε ∈ Δ or ε ≡ p ∈ Δ and ç ∈ p, fail.

3) If p ≡ ε ∈ Δ or ε ≡ p ∈ Δ and p = α1 ⋆ ⋯ ⋆ αn
 Δ ↦ {α1 ↦ ε, …, αn ↦ ε} (Δ \ {p ≡ ε})
 Add {α ↦ ε, …, αn ↦ ε} to θ

4) If an equation ρ has the form ç ⋆ p1 ≡ ç ⋆ p2, or
 α ⋆ p1 ≡ α ⋆ p2 then replace ρ with p1 ≡ p2

5) If an equation ρ has the form ç1 ⋆ ⋯ ⋆ çn ≡ ç ⋆ p, (and by rule 5 none
 of the çi appear in p) then fail.

6) If an equation ρ has the form α ⋆ çs ≡ αs ⋆ çs', where there are no
 variables in çs and |çs| > 0, then {α ↦ α' ⋆ çs'} where α' is a new variable.
 (Recall that by now çs ∩ çs' = ∅)

7) Otherwise, either ρ has the form
 ç1 ⋆ ⋯ ⋆ çn ≡ αs
 where αs consists of at least two variables
 or both sides of ρ has one of the following forms
 α1 ⋆ α2 ⋆ w, α ⋆ ç ⋆ w
 In this case, we return ρ as a constraint.
*)

structure Entails :> Entails = struct
   structure C = CFormula
   structure T = Term
   structure S = Term.Set
   structure W = World
   structure Eqs = W.Eqs
   structure LEqs = LW.Eqs

   open General
   open PP.Ops

   datatype res =
      Changed of Subst.t * Eqs.t * LEqs.t
    | Unchanged
    | Fail

   (*
      ε ≡ ε ---> ⊤
      ε ≡ α · p ---> α ↦ ε, ε ≡ p
      ε ≡ c · p ---> fail
      ε ≡ ι w · p ---> w ≡ ε, ε ≡ p
    *)
   fun phase1 eqs =
      let
         fun ffn (w1, w2) =
            if W.isEps w1 then SOME (w1, w2) else
            if W.isEps w2 then SOME (w2, w1) else
            NONE
      in
         case List.findRemFirst ffn eqs of
            NONE => Unchanged
          | SOME ((_, r), eqs) =>
            case Seq.viewl (W.seq w) of
               EmptyL => Changed (Subst.id, eqs, [])
             | Cons (x, w) =>
               case x of
                  T.Var x =>
                  Changed (Subst.sing (Left (x, W.epsT)), (W.eps, w) :: eqs, [])
                | T.Param _ => Fail
                | T.Fn (f, [x]) =>
                  if Func.eq (f, Func.Ordered.inj) then
                     Changed (Subst.id, (W.eps, w) :: eqs, [(x, LW.eps)])
                  else Fail
                | T.Fn _ => Fail
      end

   val phases = [ phase1 ]

   val unifyEqs =
      let
         fun loop (s, eqs) = fn
            [] => SOME (s, eqs)
          | p :: ps =>
            case p eqs of
               Fail => NONE
             | Unchanged => loop (s, eqs) ps
             | Changed (s', eqs) =>
               let
                  fun mfn1 t = Subst.apply (t, s')
                  fun mfn2 s = W.map mfn1 s
                  fun mfn3 (s1, s2) = (mfn2 s1, mfn2 s2)
                  val eqs = List.map mfn3 eqs
               in
                  loop (Subst.compose (s, s'), eqs) phases
               end
      in
         fn eqs => loop (Subst.id, eqs) phases
      end

   val simp =
      let
         fun quant q ((x, s), f) =
            if Atoms.mem (C.atoms f, Left x) then
               let
                  val (t, f) = simp f
               in
                  (t, q ((x, s), f))
               end
            else simp f
         and simp f = case f of
            C.Ex (x, f) => quant C.Ex (x, f)
          | C.All (x, f) => quant C.All (x, f)
          | C.Bot => (Subst.id, C.Bot)
          | C.Top => (Subst.id, C.Top)
          | _ =>
            if C.propositional f then
               case unifyEqs (Eqs.ofForm f) of
                  SOME (s, eqs) => (s, Eqs.toForm eqs)
                | NONE => (Subst.id, C.Bot)
            else (Subst.id, f)
      in
         fn f => simp (C.simplify f)
      end

   val valid =
      let
         val rec f = fn
            C.Top => true
          | C.And (a, b) => f a andalso f b
          | C.Bot => false
          | _ => false
      in
         f
      end

   fun unify eq =
      let
         val (t1, t2) = eq
         (* val _ = PP.ppl (%%[$"Unify:", Term.pp t1, $",", Term.pp t2]) *)
         val res = case unifyEqs (Eqs.ofEqs [eq]) of
            SOME (s, eqs) => SOME (CSubst.make (s, Eqs.toEqs eqs))
          | NONE => NONE
      in
         Log.trace (fn () =>
            (&[ %%[$"Unify:", Term.pp t1, $",", Term.pp t2]
              , %%[$"Res  :", PP.option CSubst.pp res]]));
         res
      end

   fun f {entailed, entailer, global = _} = case (entailed, entailer) of
      (_, C.Bot) => true
    | (C.Top, f) => valid f
    | _ => false

   val reduce = W.reduce
end
