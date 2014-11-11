
-- | Modal logic constraint solver.

{- 

-} 

-- @ Pragmas

{-# LANGUAGE CPP, Rank2Types, FlexibleContexts, MultiParamTypeClasses #-} 

-- @ Signature

module Imogen.Modal.Solver
  ( -- * Solver
    solver
  , simplify
  )
where

-- @ Imports

#include "../../undefined.h" 

import Imogen.Util.Prelude hiding (null, seq, concat)
import qualified Control.Monad.State as State
import qualified Data.Maybe as Maybe
import qualified Imogen.Atom as Atom
import qualified Imogen.Constr as C
import Imogen.Constr (Ψ, (⊤), (∧))
import qualified Imogen.CSubst as CSubst
import qualified Imogen.Modal as Modal
import Imogen.Modal (Eqn)
import qualified Imogen.Pred as Pred
import qualified Imogen.Term as Term
import Imogen.Term (Encode(..))
import qualified Imogen.Util.Debug as Debug
import qualified Imogen.Util.Fresh as Fresh
import Imogen.Util.Fresh (HasFresh)
import qualified Imogen.Util.List as List
import qualified Imogen.Util.Log as Log
import Imogen.Util.Log (Log)
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Print as PP
import qualified Imogen.Var as Var
import Imogen.Var (Var)

-- @ Normal forms

{-

Constraints are either ⊥ or a pair of dependency constraints and path equations.

Constrants     Ψ ::= ⊥ | Δ ; Σ 
Dependencies   Δ ::= · | π[e1 … en], Δ 
Equations      Σ ::= · | Π1 ≡ Π2 ∧ Σ
Paths          Π
Path variables π 
Edges          e

The normal form encoding of constraints has one of the following forms:

⊥ 
(canDepend(π1, e1, e2, …, en) ∧ … ∧ canDepend(πn, e1, e2, …, en)) ∧ 
(Π1 ≡ Π1' ∧ … ∧ Πn ≡ Πn')

We need ⊥ since it comes from the initial sequent ⊥ | · ⊢ ·
All other sequents have the second form.  

To get to this normal form, we need to normalize since during
proof search we modify the constraints of the input sequent.

 Ψ ::= canDepend(π, es) | Π1 ≡ Π2 | Ψ ∧ Ψ | ∀ e. Ψ | ∃ π. Ψ 

To normalize a constraint we will first translate it to the compact
form, then encode it back to a constraint.
-} 

-- | Dependency  π1[e1, …, en]
type Dep = (Var, [Param])

-- | Normal form for constraints
data Normal = Inconsis
            | Consis ([Dep], [Eqn])

toConstr :: Normal -> C.Formula
toConstr Inconsis = C.Bot
toConstr (Consis (deps, eqs)) = (C.listAnd (map mkDep deps) ∧ C.listAnd eqns)
 where
  mkDep (π, es) = Atom.Rel (Pred.make "dep") (encode x : map encode es)
  eqns = map (C.Atom . Modal.mkEqn) eqs

fromConstr :: C.Formula -> Normal
fromConstr C.Bot = Inconsis
fromConstr ψ = (es, ws', eqs')
 where
  (es, _, b) = C.destAll ψ
  (ws, eqs) = case b of
                And ws eqs -> (ws, eqs)
                _ -> error' $ PP.text "Non-normal form: " <+> pPrint ψ
  ws' = map Modal.destPath (C.destAnd ws)
  eqs' = map Modal.destEqn (C.destAnd eqs')

{- 
Try to discharge path constraints.

We can remove path constraints if they have the form
path(π0 ⋆ e1 ⋆ e2 ⋆ ⋯ ⋆ en) where the ei are all elements
of the quantified edges.  
-} 
simplifyPathConstrs :: Normal -> Normal
simplifyPathConstrs (es, ps, eqs) = (es, ps', eqs)
 where 
  ps' = filter (not . legal) ps
  legal w = 
    let getVar (Modal.V x) = Just x
        getVar _ = Nothing
        vs = mapMaybe getVar (Modal.toList w)
    in all (flip elem es) vs

-- Try to discharge equational constraints.
simplifyEqnConstrs :: Normal -> Maybe (Normal, CSubst.Θ)
simplifyEqnConstrs (_, _, eqs) = 
  

{- 
Once a formula is in normal
form, we can simplify it in the following ways:

0) If e doesn't appear in the body, eliminate it.

1) If αi ≡ βi ∈ B has the form x ≡ ω, then we can remove the equation,
   eliminate x, and apply {x ↦ ω}

2) Otherwise all equations have the form α1 · x · α2 ≡ β.  Here we need to fail
   and leave the existential constraint.
-} 
simplify :: (MonadState s m, HasFresh Var s, Modal.HasMode s, Log m) => Σ -> Ψ -> m (Maybe (Θ, Ψ))
simplify _ ψ = case fromConstr ψ of
  Inconsis -> return Nothing
  Consis norm -> 
    let (es, ps, eqs) = simplifyPathConstrs norm in do
    res <- Modal.simplify (map Modal.eqnToEqn3 eqs)
    case res of
      Nothing -> return Nothing
      Just (θ, eqs') -> 
        -- remove quantified variables that no longer appear
        let es' = List.intersect es (vars (ps, eqs))
        return $ Just (θ, toConstr (Consis (es, ps, map Modal.eqn3ToEqn eqs')))

-- @ Solver

solver :: C.Solver
solver = C.Solver "Modal" simplify valid
  valid sig ψ = do 
    s <- simplify sig ψ 
    case s of 
      Just (_, C.Top) -> return True
      _ -> return False
