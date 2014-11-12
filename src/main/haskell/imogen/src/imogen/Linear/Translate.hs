-- FIXME: DOC
-- @ Pragmas

{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

-- @ Signature

module Imogen.Linear.Translate
  ( translate
  , examples
  )
where

-- @ Imports

import Imogen.Util.Prelude
import qualified Control.Monad.State as State
import qualified Imogen.Linear.Frame as Frame
import Imogen.Linear.Frame ((◃), (⊛), (ⓦ), (ⓕ))
import qualified Imogen.Linear.LL as L
import Imogen.Linear.LL(Pos(..), Neg(..))
import qualified Imogen.Linear.World as World
import Imogen.Linear.World (World, ε, (⋆))
import qualified Imogen.PFormula as F
import Imogen.PFormula ( (↓), (⊤), (∧), (⊃), (¥) )
import qualified Imogen.Sort as Sort
import Imogen.Sort (Base(LWorld, Head, U))
import qualified Imogen.Util.Fresh as Fresh
import Imogen.Util.Fresh (HasFresh)
import qualified Imogen.Util.Parse as Parse
import qualified Imogen.Var as Var
import Imogen.Var (Var)

-- @ Translation

translate :: (MonadState s m, HasFresh Var s) => Neg -> m F.Neg
translate n = atWorld n ε

{-
  P           P @ k
----------------------------------------
  0           ⊤
  P1 ⊕ P2     P1 @ k ∧ P2 @ k
  1           k(ε)
  P1 ⊗ P2     P2 @ α. P1 @ β. k(α ⋆ β)
  a+          ∀ α. a+ @ α ⊃ k(α)
  ↓ N         ∀ α. ↓ (N @ α) ⊃ k(α)
  ! N         ↓ (N @ ε) ⊃ k(ε)
  ∃ x. N      ∀ x. P @ k
-}

atCont :: (MonadState s m, HasFresh Var s) => Pos -> (World -> m F.Neg) -> m F.Neg
atCont Zero _ = return (⊤)
atCont (Sum p1 p2) k = do
  p1' <- atCont p1 k
  p2' <- atCont p2 k
  return (p1' ∧ p2')
atCont One k = k ε
atCont (Tensor p1 p2) k =
  atCont p2 (\α -> atCont p1 (\β -> k (α ⋆ β)))
atCont (PAtom a) k = do
  α <- Fresh.fresh "α"
  let α' = World.var α
  f <- k α'
  return $ (¥) α LWorld ((F.PAtom (a ⓦ α')) ⊃ f)
atCont (Down n) k = do
  α <- Fresh.fresh "α"
  n' <- atWorld n (World.var α)
  f <- k (World.var α)
  return $ (¥) α LWorld ((↓) n' ⊃ f)
atCont (Bang n) k = do
  n' <- atWorld n ε
  p <- k ε
  return $ (↓) n' ⊃ p
atCont (Ex x p) k = do
  n' <- atCont p k
  return $ (¥) x U n'

{-
Note that for the negative translation we need to
modify the ∀ frame variable quantifier since we don't
;allow frame variables.  For instance, in the paper the rule
for negative atoms is

  a-    ↦    ∀ φ. a- @ φ ⊃ φ ◃ ρ

We'll translate this as

  a-    ↦    ∀ φ α. a- @ (φ ⊛ α) ⊃ (φ ⊛ α) ◃ ρ

where φ is now a Head variable and α is a World variable.

  N           N @ ρ
---------------------------------------------
  ⊤          ⊤
  N1 ∧ N2    N1 @ ρ ∧ N2 @ ρ
  P ⊸ N      P @ α. N @ (p ⋆ α)
  a-         ∀ φ α. a- @ (φ ⊛ α) ⊃ (φ ⊛ α) ◃ ρ
  ↑P         ∀ h α. ↓(P @ α'. (h ⊛ α) ◃ α') ⊃ (h ⊛ α) ◃ ρ
  ∀ x. N     ∀ x. N @ p
-}

atWorld :: (MonadState s m, HasFresh Var s) => Neg -> World -> m F.Neg
atWorld Top _ = return F.Top
atWorld (With n1 n2) ρ = do
  n1' <- atWorld n1 ρ
  n2' <- atWorld n2 ρ
  return $ n1' ∧ n2'
atWorld (Lolli p n) ρ = do
  atCont p (\α -> atWorld n (ρ ⋆ α))
atWorld (NAtom a) ρ = do
  φ <- Fresh.fresh "φ"
  α <- Fresh.fresh "α"
  let f = Frame.var φ ⊛ World.var α
  return $ (¥) φ Head $ (¥) α LWorld $ F.PAtom (a ⓕ f) ⊃ F.NAtom (f ◃ ρ)
atWorld (Up p) ρ = do
  φ <- Fresh.fresh "φ"
  α <- Fresh.fresh "α"
  let f = Frame.var φ ⊛ World.var α
  p' <- atCont p (\α' -> return $ F.NAtom (f ◃ α'))
  return $ (¥) φ Head $ (¥) α LWorld ((↓) p' ⊃ F.NAtom (f ◃ ρ))
atWorld (All x p) ρ = do
  p' <- atWorld p ρ
  return $ (¥) x U p'

-- @ Examples

newtype VarState = S Var

instance HasFresh Var VarState where
  nextFresh name (S v) = (v', S v')
    where v' = Var.next name v

parseTrans :: String -> (Neg, F.Neg)
parseTrans s =
  let t :: Neg
      t = Parse.parse s
  in (t, State.evalState (translate t) (S $ Var.start "x"))

examples :: [(Neg, F.Neg)]
examples = map parseTrans
  [ "↑ 1"
  , "↓ ↑ 1 ⊸  ↓ ↑ 1 ⊸ ↑ 1"
  , "p ⊗ q ⊸ q ⊗ p"
  , "(a ⊸ 1) ⊸ a ⊸ 0"
  , "(A ⊸ 1) ⊸ A ⊸ 0"
  ]
