-- FIXME: DOC
-- @ Pragmas

{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

-- @ Signature

module Imogen.Ordered.Translate 
  ( translate 
  , examples
  )
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as State
import qualified Imogen.Linear.World as LWorld
import qualified Imogen.Ordered.Frame as Frame
import Imogen.Ordered.Frame ((◃), (ⓛ), (ⓡ), (ⓦ), (ⓕ)) 
import qualified Imogen.Ordered.OL as L
import Imogen.Ordered.OL(Pos(..), Neg(..))
import qualified Imogen.Ordered.World as W
import Imogen.Ordered.World (World, ε, (·), ι)
import qualified Imogen.PFormula as F
import Imogen.PFormula ( (↓), (⊤), (∧), (⊃), (¥) )
import qualified Imogen.Sort as Sort
import Imogen.Sort (Base(OWorld, LWorld, Head, U))
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
  P1 ● P2     P1 @ α. P2 @ β. k(α · β)
  a+          ∀ α. a+ @ α ⊃ k(α)
  ↓ N         ∀ α. ↓ (N @ α) ⊃ k(α)
  ! N         ↓ (N @ ε) ⊃ k(ε)
  ¡ N         ∀ α'. ↓ (N @ i α') ⊃ k (i α')
  ∃ x. N      ∀ x. P @ k

-} 

atCont :: (MonadState s m, HasFresh Var s) => Pos -> (World -> m F.Neg) -> m F.Neg
atCont Zero _ = return (⊤)
atCont (Sum p1 p2) k = do
  p1' <- atCont p1 k
  p2' <- atCont p2 k
  return (p1' ∧ p2')
atCont One k = k ε
atCont (Dot p1 p2) k = 
  atCont p1 (\α -> atCont p2 (\β -> k (α · β)))
atCont (PAtom a) k = do
  α <- Fresh.fresh "α"
  let α' = W.var α
  f <- k α'
  return $ (¥) α OWorld ((F.PAtom (a ⓦ α')) ⊃ f)
atCont (Down n) k = do
  α <- Fresh.fresh "α"
  n' <- atWorld n (W.var α)
  f <- k (W.var α) 
  return $ (¥) α OWorld ((↓) n' ⊃ f)
atCont (Bang n) k = do
  n' <- atWorld n ε 
  p <- k ε
  return $ (↓) n' ⊃ p
atCont (UBang n) k = do
  α <- Fresh.fresh "α"
  n' <- atWorld n (ι $ LWorld.var α)
  p <- k (ι $ LWorld.var α)
  return $ (¥) α LWorld $ (↓) n' ⊃ p
atCont (Ex x p) k = do
  n' <- atCont p k
  return $ (¥) x U n'

{- 
Note that for the negative translation we need to 
modify the ∀ frame variable quantifier since we don't
allow frame variables.  For instance, in the paper the rule
for negative atoms is

  a-    ↦    ∀ φ. a- @ φ ⊃ φ ◃ ρ

We'll translate this as

  a-    ↦    ∀ φ αl αr. a- @ (αl ⊙ φ ⊙ αr) ⊃ (αl ⊙ φ ⊙ αr) ◃ ρ

where φ is now a Head variable and αl and αr are World variables.

  N           N @ ρ
---------------------------------------------
  ⊤          ⊤
  N1 ∧ N2    N1 @ ρ ∧ N2 @ ρ
  P >-> N    P @ α. N @ (α · p)
  P ->> N    P @ α. N @ (p · α)
  a-         ∀ φ αl αr. a- @ (αl ⊙ φ ⊙ αr) ⊃ (αl ⊙ φ ⊙ αr) ◃ ρ
  ↑P         ∀ φ αl αr. ↓(P @ α. (αl ⊙ φ ⊙ αr ◃ α)) ⊃ (αl ⊙ φ ⊛ αr) ◃ ρ 
  ∀ x. N     ∀ x. N @ p
-} 

atWorld :: (MonadState s m, HasFresh Var s) => Neg -> World -> m F.Neg
atWorld Top _ = return F.Top
atWorld (And n1 n2) ρ = do
  n1' <- atWorld n1 ρ
  n2' <- atWorld n2 ρ
  return $ n1' ∧ n2'
atWorld (ImpL p n) ρ = atCont p (\α -> atWorld n (α · ρ))
atWorld (ImpR p n) ρ = atCont p (\α -> atWorld n (ρ · α))
atWorld (NAtom a) ρ = do
  φ <- Fresh.fresh "φ" 
  αl <- Fresh.fresh "α"
  αr <- Fresh.fresh "α"
  let f = W.var αl ⓛ Frame.var φ ⓡ W.var αr
  return $ (¥) φ Head $ (¥) αl OWorld $ (¥) αr OWorld $ F.PAtom (a ⓕ f) ⊃ F.NAtom (f ◃ ρ)
atWorld (Up p) ρ = do
  φ <- Fresh.fresh "φ"
  αl <- Fresh.fresh "α"
  αr <- Fresh.fresh "α"
  let f = W.var αl ⓛ Frame.var φ ⓡ W.var αr
  p' <- atCont p (\α -> return $ F.NAtom (f ◃ α))
  return $ (¥) φ Head $ (¥) αl OWorld $ (¥) αr OWorld $ (↓) p' ⊃ F.NAtom (f ◃ ρ)
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
