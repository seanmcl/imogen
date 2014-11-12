
(*** Classical logic ***)

structure Classical :> Classical = struct
   structure F = Formula

   open General
   open Formula.Export

   val simplify =
      let
         fun simp1 fm = case fm of
            Not Bot => Top
          | Not Top => Bot
          | And (Bot, _) => Bot
          | And (_, Bot) => Bot
          | And (Top, q) => q
          | And (p, Top) => p
          | Or (Bot, q) => q
          | Or (p, Bot) => p
          | Or (Top, _) => Top
          | Or (_, Top) => Top
          | Imp (Bot, _) => Top
          | Imp (Top, q) => q
          | Imp (_, Top) => Top
          | Imp (p, Bot) => Not p
          | Iff (Top, q) => q
          | Iff (p, Top) => p
          | Iff (Bot, Bot) => Top
          | Iff (Bot, q) => Not q
          | Iff (p, Bot) => Not p
          | All ((x, _), p) => if Atoms.mem (F.atoms p, Left x) then fm else p
          | Ex ((x, _), p) => if Atoms.mem (F.atoms p, Left x) then fm else p
          | fm => fm
         val rec simp = fn
            Not p => simp1 (Not (simp p))
          | And (p, q) => simp1 (And (simp p, simp q))
          | Or (p, q) => simp1 (Or (simp p, simp q))
          | Imp (p, q) => simp1 (Imp (simp p, simp q))
          | Iff (p, q) => simp1 (Iff (simp p, simp q))
          | All (x, p) => simp1 (All (x, simp p))
          | Ex (x, p) => simp1 (Ex (x, simp p))
          | fm => fm
      in
         simp
      end
   val () = noWarnUnused (simplify)

   val rec nnf = fn
      And (p, q) => And (nnf p, nnf q)
    | Or (p, q) => Or (nnf p, nnf q)
    | Imp (p, q) => Or (nnf (Not p), nnf q)
    | Iff (p, q) => Or (And (nnf p, nnf q), And (nnf (Not p), nnf (Not q)))
    | Not (Not p) => nnf p
    | Not (And (p, q)) => Or (nnf (Not p), nnf (Not q))
    | Not (Or (p, q)) => And (nnf (Not p), nnf (Not q))
    | Not (Imp (p, q)) => And (nnf p, nnf (Not q))
    | Not (Iff (p, q)) => Or (And (nnf p, nnf (Not q)), And (nnf (Not p), nnf q))
    | All (x, p) => All (x, nnf p)
    | Ex (x, p) => Ex (x, nnf p)
    | Not (All (x, p)) => Ex (x, nnf (Not p))
    | Not (Ex (x, p)) => All (x, nnf (Not p))
    | fm => fm

   (*
   Avigad-Glivenko-Orevkov

   "The short story is this: if you put the classical formulas in
   negation-normal form first, then you can basically get away with one
   negation for every quantifier alternation.

   In fact, there is an even more remarkable translation described here:

    http://www.andrew.cmu.edu/user/avigad/Papers/cutelim.pdf

   See Section 6. Roughly: put the formula in negation normal form,
   negate it classically, then negate it intuitionistically. Then the
   original formula is classical provable iff the translation is
   intuitionistically provable. This translation is not so good for
   translating proofs, but for proof search, it should be optimal. What
   it comes down to is this: classical logic is basically intuitionistic
   logic, on the left side of the sequent arrow.

   Now that I think of it, this last one is probably what you should use."
    -- Jeremy Avigad, personal communication
    *)

   val doubleNegate = Not o nnf o Not
end
