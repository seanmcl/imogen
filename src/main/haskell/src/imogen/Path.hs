-- FIXME: DOC
-- | Paths

-- @ Pragmas

{-# LANGUAGE CPP, FlexibleInstances, TypeSynonymInstances #-}

-- @ Signature

module Imogen.Path
  ( Path
  , PathElem(..)
  , PathAtom(..)
  , Atoms, atoms
  , PathLabels, labels
  , HasLabels(..)
  , emptyLabels
  , getLabels
  , label
  ) 
where

-- @ Imports 

#include "../undefined.h" 

import Imogen.Util.Prelude hiding (lookup)
import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Data.Map as Map
import Imogen.Atom (Atom(Rel))
import qualified Imogen.Class as Class
import qualified Imogen.CSubst as CSubst
import Imogen.CSubst((○), (✴))
import Imogen.PFormula (Pos(..), Neg(..))
import qualified Imogen.Param as Param
import Imogen.Param (Params(..), Petrify(..))
import qualified Imogen.Pred as Pred
import Imogen.Pred (Pred)
import Imogen.Rename (Rename(rename))
import qualified Imogen.Sig as Sig
import Imogen.Sig (Polarity(..))
import qualified Imogen.Subst as Subst
import Imogen.Subst (Apply)
import qualified Imogen.Term as Term
import Imogen.Term (Term)
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∅))
import qualified Imogen.Var as Var
import Imogen.Var (Vars(..))

-- @ Paths

-- Paths uniquely identify subformulas of the input formula.

data PathElem = PAndL | PAndR
              | PTensorL | PTensorR
              | POrL | POrR
              | PImpL | PImpR
              | PIffL | PIffR
              | PAll | PEx
              | PUp | PDown
              | PBox Term | PDiamond Term
  deriving (Eq, Ord, Show)

type Path = [PathElem]

{- 
The result of the path analysis is, for each path, a new label (name),
the list of free variables in a fixed order, and the sort signature of
the variables.
-} 

instance Apply PathElem where
  e ✴ θ = case e of
    PBox t -> PBox $ t Subst.✴ θ 
    PDiamond t -> PDiamond $ t Subst.✴ θ 
    _ -> e

instance Vars PathElem where
  vars e = case e of
    PBox t -> vars t
    PDiamond t -> vars t
    _ -> (∅)
  free = vars

instance Params PathElem where
  params e = case e of
    PBox t -> params t
    PDiamond t -> params t
    _ -> (∅)

instance Rename PathElem where
  rename e = case e of
    PBox t -> rename t >>= return . PBox
    PDiamond t -> rename t >>= return . PDiamond
    _ -> return e

instance Petrify PathElem where
  petrify ρ e = case e of
    PBox t -> PBox $ petrify ρ t
    PDiamond t -> PDiamond $ petrify ρ t
    _ -> e

-- @ Path atoms

data PathAtom = Π Path [Term]
  deriving (Eq, Ord, Show)

instance Apply PathAtom where
  Π π t ✴ θ = Π (π Subst.✴ θ) (t Subst.✴ θ)

instance Vars PathAtom where
  vars (Π π t) = vars (π, t)
  free = vars

instance Params PathAtom where
  params (Π π t) = params (π, t)

instance Rename PathAtom where
  rename (Π π t) = do 
    π' <- rename π 
    t' <- rename t
    return $ Π π' t'

instance Class.Normalize PathAtom where
  normalize _ _ = id

instance Petrify PathAtom where
  petrify ρ (Π π t) = Π (petrify ρ π) (petrify ρ t)

-- @ Unification

{- 
To unify PathElems, we make sure all the paths line
up, and unify the world variables.
-} 

instance Class.Unif PathElem where
  unify πs = maybe (return Nothing) Class.unify (collect πs)
  match πs = maybe (return Nothing) Class.match (collect πs)

collect :: [(PathElem, PathElem)] -> Maybe [(Term, Term)]
collect [] = Just []
collect (eq:eqs) = case eq of
  (PBox τ1, PBox τ2) -> ((τ1, τ2) :) <$> collect eqs
  (PDiamond τ1, PDiamond τ2) -> ((τ1, τ2) :) <$> collect eqs
  (π1, π2)
     | π1 == π2 -> collect eqs
     | otherwise -> fail ""

instance Class.Unif PathAtom where
  unify eqs = 
    let (πs, τs) = unzip $ map f eqs
        f (Π π1 τ1, Π π2 τ2) = ((π1, π2), (τ1, τ2))
    in do r1 <- Class.unify πs
          case r1 of 
            Nothing -> return Nothing
            Just θ -> do
              r2 <- Class.unify (τs ✴ θ)
              case r2 of 
                Nothing -> return Nothing
                Just θ' -> return $ Just $ θ ○ θ'
  match eqs = 
    let (πs, τs) = unzip $ map f eqs
        f (Π π1 τ1, Π π2 τ2) = ((π1, π2), (τ1, τ2))
    in do r1 <- Class.match πs
          case r1 of 
            Nothing -> return Nothing
            Just θ -> do
              r2 <- Class.match (τs ✴ θ)
              case r2 of 
                Nothing -> return Nothing
                Just θ' -> return $ Just $ θ ○ θ'

-- @ Labels

newtype PathLabels = L (Map Path Pred)
  deriving Show

emptyLabels :: PathLabels
emptyLabels = L Map.empty

class HasLabels s where
  state :: s -> PathLabels

getLabels :: (MonadState s m, HasLabels s) => m PathLabels
getLabels = State.gets state

label :: (MonadState s m, HasLabels s) => Path -> m Pred
label π = do
  L m <- getLabels
  case Map.lookup π m of
    Nothing -> error "no label for path"
    Just p -> return p

labels :: Neg -> PathLabels
labels p = L $ State.evalState (labelsN [] p) Pred.start

labelsN :: Path -> Neg -> State Pred (Map Path Pred)
labelsN π f = do
  State.modify Pred.next'
  ρ <- State.get
  case f of
    NAtom (Rel p _) -> return $ Map.singleton π p
    And p q -> do
      lmap1 <- labelsN (PAndL : π) p
      lmap2 <- labelsN (PAndR : π) q
      return $ Map.insert π ρ $ Map.union lmap1 lmap2
    Top -> return $ Map.singleton π ρ
    Imp p q -> do
      lmap1 <- labelsP (PImpL : π) p
      lmap2 <- labelsN (PImpR : π) q
      return $ Map.insert π ρ $ Map.union lmap1 lmap2
    Iff p q -> do
      lmap1 <- labelsN (PIffL : π) p
      lmap2 <- labelsN (PIffR : π) q
      return $ Map.insert π ρ $ Map.union lmap1 lmap2
    All _ _ p -> do
      lmap <- labelsN (PAll : π) p
      return $ Map.insert π ρ lmap
    Up p -> do
      lmap <- labelsP (PUp : π) p
      return $ Map.insert π ρ lmap
    Box w p -> do
      lmap <- labelsN (PBox w : π) p
      return $ Map.insert π ρ lmap

labelsP :: Path -> Pos -> State Pred (Map Path Pred)
labelsP π f = do
  State.modify Pred.next'
  ρ <- State.get
  case f of
    PAtom (Rel p _) -> return $ Map.singleton π p
    Tensor p q -> do
      lmap1 <- labelsP (PTensorL : π) p
      lmap2 <- labelsP (PTensorR : π) q
      return $ Map.insert π ρ $ Map.union lmap1 lmap2
    One -> return $ Map.singleton π ρ
    Or p q -> do
      lmap1 <- labelsP (POrL : π) p
      lmap2 <- labelsP (POrR : π) q
      return $ Map.insert π ρ (Map.union lmap1 lmap2)
    Bot -> return $ Map.singleton π ρ
    Ex _ _ p -> do
      lmap <- labelsP (PEx : π) p
      return $ Map.insert π ρ lmap
    Down p -> do
      lmap <- labelsN (PDown : π) p
      return $ Map.insert π ρ lmap
    Diamond w p -> do
      lmap <- labelsP (PDiamond w : π) p
      return $ Map.insert π ρ lmap

-- @ Atomic formulas

{- 
Return the postive and negative atomic subformulas of a negative
proposition, along with a sort context for the variables.  (There are no
parameters at this point, as it's called before focusing).  This is
used for generating initital sequents.
-} 

type Atoms = (Map Pred (Set PathAtom), Map Pred (Set PathAtom))

atoms :: Neg -> Atoms
atoms = atomsN [] Pos
  where atomsN π pol form = case form of 
          NAtom (Rel p ts) -> case pol of 
            Pos -> (Map.singleton p $ Set.singleton $ Π π ts, Map.empty)
            Neg -> (Map.empty, Map.singleton p $ Set.singleton $ Π π ts)
          And p q -> join (atomsN (PAndL : π) pol p) (atomsN (PAndR : π) pol q)
          Top -> empty
          Imp p q -> join (atomsP (PImpL : π) (Sig.opp pol) p) (atomsN (PImpR : π) pol q)
          Iff p q -> join (atomsN (PIffL : π) pol p) (atomsN (PIffR : π) pol q)
          All _ _ p -> atomsN (PAll : π) pol p
          Up p -> atomsP (PUp : π) pol p
          Box t p -> atomsN (PBox t : π) pol p
        atomsP π pol form = case form of
          PAtom (Rel p ts) -> case pol of 
            Pos -> (Map.singleton p $ Set.singleton $ Π π ts, Map.empty)
            Neg -> (Map.empty, Map.singleton p $ Set.singleton $ Π π ts)
          Tensor p q -> join (atomsP (PTensorL : π) pol p) (atomsP (PTensorR : π) pol q)
          One -> empty
          Or p q -> join (atomsP (POrL : π) pol p) (atomsP (POrR : π) pol q)
          Bot -> empty
          Ex _ _ p -> atomsP (PEx : π) pol p
          Down p -> atomsN (PDown : π) pol p
          Diamond t p -> atomsP (PDiamond t : π) pol p
        join (p1, n1) (p2, n2) = 
           ( Map.unionWith Set.union p1 p2, Map.unionWith Set.union n1 n2 )
        empty = (Map.empty, Map.empty)

-- @ Printing

instance Print PathElem where
  pPrint e = 
    case e of 
      PAndL -> PP.text "∧l"
      PAndR -> PP.text "∧r"
      PTensorL -> PP.text "⊗l"
      PTensorR -> PP.text "⊗r"
      POrL -> PP.text "∨l"
      POrR -> PP.text "∨r"
      PImpL -> PP.text "⊃l"
      PImpR -> PP.text "⊃r"
      PIffL -> PP.text "⇔l"
      PIffR -> PP.text "⇔r"
      PAll -> PP.text "∀"
      PEx -> PP.text "∃"
      PBox w -> PP.text "□" <> pPrint w 
      PDiamond w -> PP.text "◇" <> pPrint w
      PDown -> PP.text "↓"
      PUp -> PP.text "↑"

instance Print PathAtom where
  pPrint (Π π []) = pPrint π
  pPrint (Π π ts) = pPrint π <> PP.tuple (map pPrint ts) 

instance Print Atoms where
  pPrint = ppAtoms pPrint

instance Print PathLabels where
  pPrint (L m) = PP.setVert $ map pp1 $ Map.toList m
    where pp1 (π, p) = pPrint π <+> PP.text "↦" <+> pPrint p

ppAtoms :: (PathAtom -> PP.Doc) -> Atoms -> PP.Doc
ppAtoms f (m1, m2) = 
  let l1 = Map.toList m1
      l2 = Map.toList m2
      pp1 (p, ats) = PP.tuple [pPrint p, PP.listVert $ map f $ Set.toList ats]
      pp m = PP.listVert $ map pp1 m
  in PP.tuple [pp l1, pp l2]
