(*
Equational theory

 ε · p ≡ p
 p · (q · r) ≡ (p · q) · r
 f ⓡ p ◃ q ≡ f ◃ q · p
 p ⓛ f ◃ q ≡ f ◃ p · q
 ι W.ε ≡ ε
 ι (p ⋆ q) ≡ ι p · ι q

Note that in the last rule p and q ∈ W.World.  Similar to linear
logic, we can write any given frame as w1 ⓛ f ⓡ w2 where f is a head.
Then we need only solve the world unification problem.  We take a
solver for W.Worlds as given, though of course this procedure also
has unsolvable parts which get added to the constraints.

legend:  α : variable
         c : constant (or parameter) (not an ι)
         ĉ : ι-constant (constant or ι)
         w : W.World

All rules are written for the left side.  They work equally
well on the right side, with the order of arguments reversed.

** Combine ι-s

w1 · ι p · ι q · w2 ---> w1 · ι (p ⋆ q) · w2

** Epsilon

ε ≡ ε ---> ⊤
ε ≡ α · p ---> α ↦ ε, ε ≡ p
ε ≡ c · p ---> fail
ε ≡ ι w · p ---> w ≡ ε, ε ≡ p

** Singleton

α ≡ w ---> α ↦ w
c ≡ α1 ⋯ c ⋯ αn ---> α1 ⋯ αn ≡ ε
c ≡ α1 ⋯ c' ⋯ αn ---> fail
c ≡ α1 ⋯ ι α ⋯ αn ---> fail
c ≡ α1 ⋯ c1 ⋯ c2 ⋯ αn ---> fail
c ≡ c · p ---> p ≡ ε
c ≡ c' · p ---> fail
c ≡ ι w · p ---> fail
ι w ≡ p1 · c · p2 ---> fail
ι w ≡ α · ι w' · p ---> α ↦ ι α', ι w ≡ ι (α' ⋆ w') · p
ι w ≡ α1 · ι w1 · α2 · ι w2 · ⋯ · αN ---> αi ↦ ι αi', w ≡ α1' ⋆ ⋯ ⋆ αN' ⋆ w1 ⋆ ⋯ ⋆ wN

** General

At this point we have at least 2 elements on each side of the
equations.  We consider the first symbols of the equality.

Constant-Constant

c · p ≡ c · q ---> p ≡ q
c · p ≡ c' · q ---> fail

Constant-W.World

c · p ≡ ι p · q ---> fail

Constant-Variable

c · p ≡ α · ĉ · q ---> α ↦ c · α', p ≡ α' · q
c · p ≡ α · α' · q (Not unitary)

Now the first symbol of either side of the equation is a variable or W.World
The possible forms of one side are

ι w · α · p ≡
ι w · c · p ≡
α · ι w · c · p ≡
α1 · ι w · α2 · p ≡
α1 · α2 · p ≡

Thus there are 15 possibilities (up to symmetry), only one of which is
unitarily reducible.

 1) ι w1 · α1 · p ≡ ι w2 · α2 · q  (No solution)

 2) ι w1 · α · p ≡ ι w2 · c · q (Not unitary)
    In this case α ≡ ε or α ≡ c · α' or α ≡ ι α3 · α'

 3-5) ι w1 · α1 · p ≡ α2 · ι w2 · c · q (Hopeless)

 6) ι w1 · c1 · p ≡ ι w2 · c2 · q (Reducible!)
    ι w1 · c1 · p ≡ ι w2 · c2 · q ---> w1 ≡ w2, c1 · p ≡ c2 · q

 7-9) ι w1 · c1 · p ≡ ι w2 · c2 · q (Hopeless)

 10-15) (Hopeless)
*)
structure Entails = struct
   structure C = CFormula
   structure T = Term
   structure S = Dlist
   structure W = World
   structure Eqs = W.Eqs
   structure LW = Linear.World
   structure LEqs = Linear.World.Eqs

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
          | SOME ((_, w), eqs) =>
            case S.viewl (W.toDlist w) of
               S.EmptyL => Changed (Subst.id, eqs, [])
             | S.Cons (x, w) =>
               case x of
                  T.Var x =>
                  Changed (Subst.sing (Left (x, W.epsT)),
                           (W.eps, W.ofDlist w) :: eqs, [])
                | T.Param _ => Fail
                | T.Fn (f, [x]) =>
                  if Func.eq (f, Func.Ordered.inj) then
                     Changed (Subst.id, (W.eps, W.ofDlist w) :: eqs,
                              [(LW.ofTerm x, LW.eps)])
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
             | Changed (s', eqs, leqs) =>
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
          | C.imogen.And (a, b) => f a andalso f b
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
