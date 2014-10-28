
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
   structure W = World
   structure Eqs = W.Eqs

   open General
   open PP.Ops

   datatype res =
      Changed of Subst.t * Eqs.t
    | Unchanged
    | Fail

   (*
      Δ = α ≡ p, Δ' or Δ = p ≡ α, Δ'
      Δ ↦ Δ'
      θ ↦ θ ○ {α ↦ p}
    *)
   fun phase1 eqs =
      let
         fun ffn (w1, w2) =
            case W.getVar w1 of
               SOME x => SOME (x, w2)
             | NONE =>
               case W.getVar w2 of
                  SOME y => SOME (y, w1)
                | NONE => NONE
      in
         case List.findRemFirst ffn eqs of
            NONE => Unchanged
          | SOME ((x, t), eqs) =>
            Changed (Subst.sing (Left (x, W.toTerm t)), eqs)
      end

   (*  If p ≡ ε ∈ Δ or ε ≡ p ∈ Δ and c ∈ p, fail. *)
   fun phase2 eqs =
      let
         fun efn (w1, w2) =
            (W.isEps w1 andalso W.hasConst w2)
            orelse
            (W.isEps w2 andalso W.hasConst w1)
      in
         if List.exists efn eqs then Fail else Unchanged
      end

   (*
      If Δ = p ≡ ε, Δ' or Δ = ε ≡ p, Δ' and p = α1 ⋆ ⋯ ⋆ αn
      θ = {α ↦ ε, …, αn ↦ ε}
   *)
   fun phase3 eqs =
      let
         fun uni (w1, w2) =
            if W.isEps w1 andalso W.all T.isUnfixedVar w2 then
               let
                  fun ffn (x, s) = Subst.extend (s, Left (T.getVarExn x, W.epsT))
               in
                  SOME (foldl ffn Subst.id (W.toList w2))
               end
            else NONE
         fun ffn (w1, w2) = case uni (w1, w2) of
            SOME s => SOME s
          | NONE => uni (w2, w1)
      in
         case List.findRemFirst ffn eqs of
            NONE => Unchanged
          | SOME (s, eqs) =>
            Changed (s, Eqs.apply (eqs, s))
      end

   (*
      If an equation ρ has the form x ⋆ p1 ≡ x ⋆ p2,
      then replace ρ with p1 ≡ p2.
   *)
   fun phase4 eqs =
      let
         fun simp (w1, w2) =
            let
               fun ffn t = W.exists (fn t' => Term.eq (t, t')) w2
            in
               case W.findRem ffn w1 of
                  NONE => NONE
                | SOME (t, w1) => SOME (w1, W.removeExn (w2, t))
            end
      in
         case List.findRemFirst simp eqs of
            NONE => Unchanged
          | SOME (eq, eqs) => Changed (Subst.id, eq :: eqs)
      end

   (*
      If an equation ρ has the form c1 ⋆ ⋯ ⋆ çn ≡ ç ⋆ p, (by the previous rulle
      none of the çi appear in p) then fail.
   *)
   fun phase5 eqs =
      let
         fun g (w1, w2) =
            not (W.exists T.isUnfixedVar w1) andalso
            W.exists (fn t => not (W.mem (w1, t))) w2
         fun ffn (w1, w2) = g (w1, w2) orelse g (w2, w1)
      in
         case List.find ffn eqs of
            NONE => Unchanged
          | SOME _ => Fail
      end

   (*
      If an equation ρ has the form α ⋆ cs ≡ αs ⋆ cs', where there are no
      variables in cs (and cs and cs' are disjoint by phase 4),
      then {α ↦ α' ⋆ cs'} where α' is a new variable, and
      replace ρ by α' * cs ≡ αs.
   *)
   fun phase6 eqs =
      let
         fun g (w1, w2) =
            if W.numVars w1 = 1 andalso W.hasConst w2 then
               let
                  val (vs1, cs1) = W.partition T.isUnfixedVar w1
                  val (vs2, cs2) = W.partition T.isUnfixedVar w2
                  val _ = assert' (fn () => W.size vs1 = 1);
                  val _ = assert' (fn () => W.size cs2 > 0);
                  val v' = Var.next ()
                  val w1 = W.add (cs1, T.Var v')
                  val w2 = vs2
                  val v = case W.getVar vs1 of
                     NONE => raise Impossible
                   | SOME v => v
                  val s = Subst.sing (Left (v, W.toTerm (W.add (cs2, T.Var v'))))
               in
                  SOME (s, (w1, w2))
               end
            else
               NONE
         fun ffn (w1, w2) = case g (w1, w2) of
            SOME x => SOME x
          | NONE => g (w2, w1)
      in
         case List.findRemFirst ffn eqs of
            NONE => Unchanged
          | SOME ((s, eq), eqs) => Changed (s, eq :: eqs)
      end

   val phases = [ phase1, phase2, phase3, phase4, phase5, phase6 ]

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

   fun simp f = case f of
      C.All ((x, _), f) =>
      if Atoms.mem (C.atoms f, Left x) then (Subst.id, f)
      else simp f
    | _ =>
      case unifyEqs (Eqs.ofForm f) of
         SOME (s, eqs) => (s, Eqs.toForm eqs)
       | NONE => (Subst.id, C.Bot)

   fun unify eq =
      let
         val (t1, t2) = eq
         val res = case unifyEqs (Eqs.ofEqs [eq]) of
            SOME (s, eqs) => SOME (CSubst.make (s, Eqs.toEqs eqs))
          | NONE => NONE
      in
         Log.trace (fn () =>
            (&[ %%[$"Unify:", Term.pp t1, $",", Term.pp t2]
              , %%[$"Res  :", PP.option CSubst.pp res]]));
         res
      end

   fun f {entailed = f1, entailer = f2, global = _} = case (f1, f2) of
      (_, C.Top) => true
    | (C.Bot, _) => true
    | _ => false

   val reduce = W.reduce
end
