
{- |
/Constraints/.

Constraints arise in a number of ways in Imogen.  

 * Unsolvable equations in non-unitary unification problems

 * Initial sequents in substructural logics

 * Modeling constraint domains (e.g. real arithmetic)

To handle the multifarious uses of constraints, we use a generic 
tree-like formula datatype (Ψ).  Constraints arising from one of the
above conditions are encoded in the constraint type.  Each Imogen
instance runs with a /constraint solver/ that iterprets the constraints
in the sequents.

Frequently, unification and constraints must work together.  For instance,
simplifying a constraint may yield an equation like x ≗ t, which then can
become a mapping in a substitution.  
-} 

-- @ Pragmas

{-# LANGUAGE CPP, Rank2Types, FlexibleContexts, DeriveDataTypeable #-} 

-- @ Signature

module Imogen.Constr 
  ( -- * Formulas
    Formula(..)
  , (⊤), (⊥), (∧), (∨), (⊃), (¬), (∃), (¥)
  , Ψ
    -- * Util
  , closeEx
  , fill
  , listAll
  , destAll
  , listAnd
  , destAnd
  , destImp
    -- * Constraint solvers
  , Solver(..)
  , dummySolver
  , HasSolver(..)
    -- ** Operations
  , simp
  , simplify
  , valid
  , contradictory
  ) 
where

-- @ Imports

#include "../undefined.h"

import Imogen.Util.Prelude hiding (lookup, init)                                   
import qualified Control.Monad.State as State
import qualified Data.Generics as G
import Imogen.Atom (Atom(..))
import Imogen.Ctx (Γ, (!))
import qualified Imogen.Modal as Modal
import qualified Imogen.Param as Param
import Imogen.Param (Params(..), Freeze(..), Petrify(..))
import qualified Imogen.Rename as Rename
import Imogen.Rename (Rename(rename))
import Imogen.Sig (Σ)
import Imogen.Sort (Base)
import Imogen.Subst (Θ, Apply, (✴), ι)
import qualified Imogen.Util.Fresh as Fresh
import Imogen.Util.Fresh (HasFresh)
import qualified Imogen.Util.List as List
import Imogen.Util.Log (Log)
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((\\), (∅))
import qualified Imogen.Var as Var
import Imogen.Var (Var, Vars(..))

-- @ Formulas

data Formula = Atom Atom                -- ^ Atomic formulas (e.g. term equality)
             | Top                      -- ^ ⊤
             | Bot                      -- ^ ⊥
             | Not Formula              -- ^ ¬ p
             | And Formula Formula      -- ^ p ∧ q
             | Or Formula Formula       -- ^ p ∨ q
             | Imp Formula Formula      -- ^ p ⊃ q
             | Iff Formula Formula      -- ^ p ⇔ q
             | All Var Base Formula     -- ∀ X : σ. p
             | Ex Var Base Formula      -- ∃ X : σ. p
  deriving (Eq, Ord, Show, Data, Typeable)

-- @@ Syntax

(⊤) :: Formula
(⊤) = Top

(⊥) :: Formula
(⊥) = Bot

(¬) :: Formula -> Formula 
(¬) = Not

(⊃) :: Formula -> Formula -> Formula 
(⊃) = Imp

(∧) :: Formula -> Formula -> Formula
(∧) = And

(∨) :: Formula -> Formula -> Formula
(∨) = Or

(∃) :: Var -> Base -> Formula -> Formula
(∃) = Ex

(¥) :: Var -> Base -> Formula -> Formula
(¥) = All

-- @@ Traversal

onAtoms :: (Atom -> Atom) -> Formula -> Formula
onAtoms afn = G.everywhere (G.mkT afn)

overAtoms :: (Atom -> a -> a) -> a -> Formula -> a
overAtoms afn b f = case f of
  Atom a -> afn a b
  Top -> b
  Bot -> b
  Not p -> overAtoms afn b p
  And p q -> overAtoms afn (overAtoms afn b p) q
  Or p q -> overAtoms afn (overAtoms afn b p) q
  Imp p q -> overAtoms afn (overAtoms afn b p) q
  Iff p q -> overAtoms afn (overAtoms afn b p) q
  Ex _ _ p -> overAtoms afn b p
  All _ _ p -> overAtoms afn b p
  Hole _ -> b

-- @@ Instances

instance Freeze Formula where
  freeze _ = __IMPOSSIBLE__ 
  thaw = onAtoms . thaw

instance Petrify Formula where
  petrify = onAtoms . petrify

-- FIXME: Should application be capture avoiding?
instance Apply Formula where
  p ✴ θ = onAtoms (✴ θ) p

instance Vars Formula where
  vars f = case f of
    Atom a -> vars a
    Top -> (∅)
    Bot -> (∅)
    Not p -> vars p
    And p q -> vars (p, q)
    Or p q -> vars (p, q)
    Imp p q -> vars (p, q)
    Iff p q -> vars (p, q)
    All x _ p -> Set.insert x (vars p)
    Ex x _ p -> Set.insert x (vars p)
    Hole _ -> (∅)
  free f = case f of
    Atom a -> free a
    Top -> (∅)
    Bot -> (∅)
    Not p -> free p
    And p q -> free (p, q)
    Or p q -> free (p, q)
    Imp p q -> free (p, q)
    Iff p q -> free (p, q)
    Ex x _σ p -> Set.delete x (free p)
    All x _σ p -> Set.delete x (free p)
    Hole _ -> (∅)

instance Params Formula where
  params = overAtoms (Set.union . params) (∅)

instance Rename Formula where
  rename f = case f of 
      Atom a -> rename a >>= return . Atom 
      Top -> return (⊤)
      Bot -> return (⊥)
      Not p -> rename p >>= return . Not
      And p q -> binop p q And 
      Or p q -> binop p q Or
      Imp p q -> binop p q Imp
      Iff p q -> binop p q Iff
      All x σ p -> exop x σ p All
      Ex x σ p -> exop x σ p Ex
      Hole _ -> return f
   where binop p q op = do
           p' <- rename p
           q' <- rename q
           return $ op p' q'
         exop x σ p op = do
           curx <- State.gets (Rename.lookup x)
           x' <- rename x
           p' <- rename p
           case curx of 
             Nothing -> State.modify (Rename.delete x)
             Just curx' -> State.modify (Rename.insert x curx')
           return $ op x' σ p'

-- @@ Util

-- | > listEx [X, Y, Z] [σ1, σ2, σ3] p == ∃ (X : σ1) (Y : σ2) (Z : σ3). p
listEx :: [Var] -> [Base] -> Formula -> Formula
listEx xs σs p = List.foldr2 Ex p xs σs

-- | > listEx [X, Y, Z] [σ1, σ2, σ3] p == ∀ (X : σ1) (Y : σ2) (Z : σ3). p
listAll :: [Var] -> [Base] -> Formula -> Formula
listAll xs σs p = List.foldr2 All p xs σs

-- | > destAll (∀ x1:σ1. ... ∀ xn:σn. b) == ([x1, ..., xn], [σ1, ..., σn], b)
destAll :: Formula -> ([Var], [Base], Formula)
destAll (All x σ p) = (x:xs, σ:σs, b)
  where (xs, σs, b) = destAll p
destAll f = ([], [], f)

-- | > listAnd [a, b, c, d] = a ∧ b ∧ c ∧ d 
listAnd :: [Formula] -> Formula
listAnd [] = Top
listAnd [p] = p
listAnd ps = foldr1 And ps

-- | > destAnd a ∧ (b ∧ c) ∧ d == [a, b, c, d]
destAnd :: Formula -> [Formula]
destAnd (And p q) = destAnd p ++ destAnd q
destAnd p = [p]

-- | > destImp a ⊃ b ⊃ c == ([a, b], c)
destImp :: Formula -> ([Formula], Formula)
destImp (Imp p q) = (p:ps, q')
  where (ps, q') = destImp q
destImp p = ([], p)

-- | Existentially quantify the unused variables in a formula.
closeEx :: Γ -> Set Var -> Formula -> Formula
closeEx ctx xs f = listEx xs' (map (ctx !) xs') f
  where xs' = Set.toList $ free f \\ xs

-- FIXME: Should 'fill' be capture avoiding?

-- | Fill a hole with a formula.
fill :: (Int, Formula) -> Formula -> Formula
fill np@(n, f') f = case f of 
  Hole n' | n == n' -> f'
          | otherwise -> f
  Top -> Top
  Bot -> Bot
  Not p -> Not (fill np p)
  And p q -> And (fill np p) (fill np q)
  Or p q -> Or (fill np p) (fill np q)
  Imp p q -> Imp (fill np p) (fill np q)
  Iff p q -> Iff (fill np p) (fill np q)
  All x σ p -> All x σ (fill np p) 
  Ex x σ p -> Ex x σ (fill np p) 
  Atom _ -> f

-- @@ Basic simplifcation

-- | Simplification that should hold of any constraint domain.

simp :: Formula -> Formula
simp fm = case fm of 
  Not p -> simp1 $ Not $ simp p
  And p q -> simp1 $ simp p ∧ simp q
  Or p q -> simp1 $ simp p ∨ simp q
  Imp p q -> simp1 $ simp p ⊃ simp q
  All x σ p -> simp1 $ All x σ $ simp p
  Ex x σ p -> simp1 $ Ex x σ $ simp p
  _ -> fm

simp1 :: Formula -> Formula
simp1 fm = case fm of
  Not Bot -> (⊤)
  Not Top -> (⊥)
  And Bot _ -> (⊥)
  And _ Bot -> (⊥)
  And Top p -> p
  And p Top -> p
  Or Bot p -> p
  Or p Bot -> p
  Or Top _ -> (⊤)
  Or _ Top -> (⊤)
  Imp Bot _ -> (⊤)
  Imp p Bot -> (¬) p
  Imp Top p -> p
  Imp _ Top -> (⊤)
  Iff p Bot -> (¬) p
  Iff Bot p -> (¬) p
  Iff Top p -> p
  Iff p Top -> p
  All x _ p -> if Set.member x (free p) then fm else p
  Ex x _ p -> if Set.member x (free p) then fm else p
  _ -> fm


-- | A convenient name since there are so many other Formulas hanging around.
type Ψ = Formula

-- @ Solver

data Solver = Solver
  { name :: String 
  , simplify' :: (MonadState s m, HasFresh Var s, Modal.HasMode s, Log m) => Σ -> Formula -> StateT Γ m (Maybe (Θ, Formula))
  , valid'    :: (MonadState s m, HasFresh Var s, Modal.HasMode s, Log m) => Σ -> Formula -> StateT Γ m Bool
  }

instance Show Solver where
  show = name

instance Print Solver where
  pPrint s = PP.text (name s ++ " : <solver>")

-- | A default solver that does nothing.
dummySolver :: Solver
dummySolver = Solver "Dummy" s v
 where 
  s _ ψ = 
    let ψ' = simp ψ in
    return $ if ψ == ψ' then Nothing else Just (ι, ψ')
  v _ ψ = 
    let ψ' = simp ψ in
    return $ ψ' == (⊤) 

class HasSolver s where
  getSolver :: s -> Solver

-- | Simplify a formula in a state with a solver.
simplify :: (MonadState s m, HasFresh Var s, Log m, Modal.HasMode s, HasSolver s) => Σ -> Formula -> StateT Γ m (Maybe (Θ, Formula))
simplify sig ψ = do
  s <- M.lift State.get
  simplify' (getSolver s) sig ψ 

-- | Check validity of a formula in a state with a solver.
valid :: (MonadState s m, HasFresh Var s, Log m, Modal.HasMode s, HasSolver s) => Σ -> Formula -> StateT Γ m Bool
valid sig ψ = do s <- M.lift State.get
                 valid' (getSolver s) sig ψ

-- | Check whether a formula is contradictory in a state with a solver.
contradictory :: (MonadState s m, HasFresh Var s, Log m, Modal.HasMode s, HasSolver s) => Σ -> Formula -> StateT Γ m Bool
contradictory sig ψ = valid sig ((¬) ψ)

-- @ Debug

instance Print Formula where
  pPrint = print' 0
   where
    print' pr f = case f of 
      Atom a -> pPrint a
      Top -> PP.text "⊤"
      Bot -> PP.text "⊥"
      Not p -> PP.paren (pr > notPrec) p'
        where p' = PP.text "¬" <+> print' (notPrec+1) p
      And p q -> PP.paren (pr > andPrec) pq
        where pq = PP.sep [PP.hsep[p', PP.text "∧"],
                           q'] 
              p' = print' (andPrec+1) p
              q' = print' andPrec q
      Or p q -> PP.paren (pr > orPrec) pq
        where pq = PP.sep [PP.hsep[p', PP.text "∨"],
                           q'] 
              p' = print' (orPrec+1) p
              q' = print' orPrec q
      Imp p q -> PP.paren (pr > impPrec) pq
        where pq = PP.sep [PP.hsep[p', PP.text "⊃"],
                           q'] 
              p' = print' (impPrec+1) p
              q' = print' impPrec q
      Iff p q -> PP.paren (pr > iffPrec) pq
        where pq = PP.sep [PP.hsep[p', PP.text "⇔"],
                           q'] 
              p' = print' (iffPrec+1) p
              q' = print' iffPrec q
      All x σ p -> PP.paren (pr > quantPrec) f'
        where f' = PP.hang (PP.text "∀" <+> PP.parens (pPrint x <+> PP.text ":" <+> PP.pPrint σ) <> PP.text ". ")
                           2 p'
              p' = print' quantPrec p
      Ex x σ p -> PP.paren (pr > quantPrec) f'
        where f' = PP.hang (PP.text "∃" <+> PP.parens (pPrint x <+> PP.text ":" <+> PP.pPrint σ) <> PP.text ". ")
                           2 p'
              p' = print' quantPrec p
      Hole n -> PP.braces $ PP.int n

    notPrec, andPrec, orPrec, impPrec, iffPrec, quantPrec :: Int
    notPrec = 9
    andPrec = 8
    orPrec = 7
    impPrec = 6
    iffPrec = 5
    quantPrec = 2
