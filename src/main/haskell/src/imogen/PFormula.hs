
{- | 
@
Polarized multi-modal first order logic.

  Worlds p ::= α | ...

  Negative Atoms p- ::= ...
  Positive Atoms p+ ::= ...

  Negative Formulas A  ::= p- 
                         | A ∧ A 
                         | ⊤ 
                         | B ⊃ A 
                         | ∀ (X1, ..., Xn : σ). A(x)
                         | ↑ B 
                         | p □ A | p ◇ A

  Positive Formulas B  ::= p+ 
                         | B ⊗ B 
                         | 1
                         | B ∨ B 
                         | ⊥
                         | ∃ (X1, ..., Xn : σ). B(x)
                         | ↓ A
@
-} 

-- @ Pragmas

-- Orphan warning for normalize hacking.  Ignore it.

{-# LANGUAGE DeriveDataTypeable #-} 
{-# OPTIONS_GHC -fno-warn-orphans #-} 

-- @ Signature

module Imogen.PFormula
  ( -- * Formulas
    -- *** Positive
    Pos(..)
  , (⊗), (∨), (⊥), (∃), (↓)
    -- *** Negative
  , Neg(..)
  , (∧), (⊤), (⊃), (⇔), (¥), (↑), (□), (◇)
    -- * Context
  , context
    -- * Util
  , listImp
    -- * Printing
  , printPos, printNeg
  ) 
where

-- @ Imports 

import Imogen.Util.Prelude hiding (pred)
import qualified Control.Monad.State as State
import qualified Data.Generics as G
import qualified Data.List as List
import qualified Imogen.Atom as Atom
import Imogen.Atom(Atom)
import qualified Imogen.Ctx as Ctx
import Imogen.Ctx (Γ)
import qualified Imogen.Func as Func
import Imogen.Func (Funcs(..))
import qualified Imogen.Param as Param
import Imogen.Param (Params, params, Petrify(..))
import qualified Imogen.Pred as Pred
import Imogen.Pred (Preds(..))
import qualified Imogen.Rename as Rename
import Imogen.Rename (Rename, rename)
import qualified Imogen.Sort as Sort
import Imogen.Sort (Base)
import Imogen.Subst(Apply, (✴))
import qualified Imogen.Term as Term
import Imogen.Term (Term)
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∅))
import qualified Imogen.Var as Var
import Imogen.Var (Var, Vars(vars, free))

-- @ Formulas

-- @@ Positive

data Pos = PAtom Atom          -- ^ Positive atomic formulas p
         | Tensor Pos Pos      -- ^ A ⊗ B
         | One                 -- ^ 1
         | Or Pos Pos          -- ^ A ∨ B
         | Bot                 -- ^ ⊥
         | Ex Var Base Pos     -- ^ ∃ x. A 
         | Diamond Term Pos    -- ^ ◇ A (or alice ◇ A)
         | Down Neg            -- ^ ↓ A    
  deriving (Eq, Ord, Show, Data, Typeable)

-- @@ Negative

data Neg = NAtom Atom          -- ^ Negative atomic formulas p
         | And Neg Neg         -- ^ A ∧ B
         | Top                 -- ^ ⊤
         | Imp Pos Neg         -- ^ A ⊃ B
         | Iff Neg Neg         -- ^ A ⇔ B
         | All Var Base Neg    -- ^ ∀ x. A
         | Up Pos              -- ^ ↑ A
         | Box Term Neg        -- ^ □ A (or alice □ A)
  deriving (Eq, Ord, Show, Data, Typeable)

-- @ Syntax

-- @@ Positive 

( ⊗ ) :: Pos -> Pos -> Pos
( ⊗ ) = Tensor

( ∨ ) :: Pos -> Pos -> Pos
( ∨ ) = Or

( ⊥ ) :: Pos
( ⊥ ) = Bot

( ∃ ) :: Var -> Base -> Pos -> Pos
( ∃ ) = Ex

( ↓ ) :: Neg -> Pos
( ↓ ) = Down

( ◇ ) :: Term -> Pos -> Pos
( ◇ ) = Diamond

-- @@ Negative

( ∧ ) :: Neg -> Neg -> Neg 
( ∧ ) = And

( ⊤ ) :: Neg
( ⊤ ) = Top

( ⊃ ) :: Pos -> Neg -> Neg
( ⊃ ) = Imp 

( ⇔ ) :: Neg -> Neg -> Neg
( ⇔ ) = Iff 

( ¥ ) :: Var -> Base -> Neg -> Neg
( ¥ ) = All

( ↑ ) :: Pos -> Neg 
( ↑ ) = Up

( □ ) :: Term -> Neg -> Neg
( □ ) = Box

-- @ Util

-- | listImp [a1, a2, a3] a4 == a1 ⊃ a2 ⊃ a3 ⊃ a4 
listImp :: [Pos] -> Neg -> Neg
listImp ps n = foldr Imp n ps

-- @ Combinators

onAtomsP :: (Atom -> Atom) -> Pos -> Pos
onAtomsP afn = G.everywhere (G.mkT afn)

onAtomsN :: (Atom -> Atom) -> Neg -> Neg
onAtomsN afn = G.everywhere (G.mkT afn)

overAtomsP :: (Atom -> a -> a) -> a -> Pos -> a
overAtomsP afn b f = case f of
  PAtom a -> afn a b
  Tensor p q -> overAtomsP afn (overAtomsP afn b p) q
  One -> b
  Or p q -> overAtomsP afn (overAtomsP afn b p) q
  Bot -> b
  Ex _ _ p -> overAtomsP afn b p
  Down p -> overAtomsN afn b p
  Diamond _ p -> overAtomsP afn b p

overAtomsN :: (Atom -> a -> a) -> a -> Neg -> a
overAtomsN afn b f = case f of 
  NAtom a -> afn a b
  And p q -> overAtomsN afn (overAtomsN afn b p) q
  Top -> b
  Imp p q -> overAtomsN afn (overAtomsP afn b p) q
  Iff p q -> overAtomsN afn (overAtomsN afn b p) q
  All _ _ p -> overAtomsN afn b p
  Up p -> overAtomsP afn b p
  Box _ p -> overAtomsN afn b p

-- @ Context

{- |
Grab the types of all the bound variables.  This assumes the bound
variables have all been renamed such that no variable name is bound
twice.  Since input formulas are closed, this will allow us to
determine the sorts of all variables and parameters during focusing.
-} 
context :: Neg -> Γ
context = ctxN 
 where 
  ctxP f = case f of 
    Tensor p q -> Ctx.join (ctxP p) (ctxP q)
    Or p q -> Ctx.join (ctxP p) (ctxP q)
    Down p -> ctxN p
    Ex x σ p -> Ctx.insert x σ (ctxP p)
    Diamond _ p -> ctxP p
    _ -> Ctx.empty
  ctxN f = case f of
    And p q -> Ctx.join (ctxN p) (ctxN q) 
    Imp p q -> Ctx.join (ctxP p) (ctxN q) 
    Up p -> ctxP p
    Box _ p -> ctxN p
    All x σ p -> Ctx.insert x σ (ctxN p)
    _ -> Ctx.empty

-- @ Instances 

instance Vars Neg where
  vars f = case f of 
    NAtom a -> vars a
    And p q -> vars (p, q)
    Top -> (∅)
    Imp p q -> vars (p, q)
    Iff p q -> vars (p, q)
    All x _ p -> Set.insert x (vars p) 
    Up p -> vars p
    Box _ p -> vars p
  free f = case f of
    NAtom a -> free a
    And p q -> free (p, q)
    Top -> (∅)
    Imp p q -> free (p, q)
    Iff p q -> free (p, q)
    All x _ p -> Set.delete x (free p) 
    Up p -> free p
    Box _ p -> free p

instance Vars Pos where
  vars f = case f of
    PAtom a -> vars a
    Tensor p q -> vars (p, q)
    One -> (∅)
    Or p q -> vars (p, q)
    Bot -> (∅)
    Ex x _ p -> Set.insert x (vars p) 
    Down p -> vars p
    Diamond _ p -> vars p
  free f = case f of
    PAtom a -> free a
    Tensor p q -> free (p, q)
    One -> (∅)
    Or p q -> free (p, q)
    Bot -> (∅)
    Ex x _ p -> Set.delete x (free p) 
    Down p -> free p
    Diamond _ p -> free p

instance Params Pos where 
  params = overAtomsP (Set.union . params) (∅)

instance Params Neg where 
  params = overAtomsN (Set.union . params) (∅)

instance Funcs Pos where 
  arityFuncs = overAtomsP (Set.union . arityFuncs) (∅)

instance Funcs Neg where 
  arityFuncs = overAtomsN (Set.union . arityFuncs) (∅)

instance Preds Pos where 
  arityPreds = overAtomsP (Set.union . arityPreds) (∅)

instance Preds Neg where 
  arityPreds = overAtomsN (Set.union . arityPreds) (∅)

instance Rename Pos where
  rename f = case f of
    PAtom a -> rename a >>= return . PAtom
    Tensor p q -> do
      p' <- rename p
      q' <- rename q
      return $ Tensor p' q'
    One -> return One
    Or p q -> do
      p' <- rename p
      q' <- rename q
      return $ Or p' q'
    Bot -> return Bot
    Down a -> rename a >>= return . Down
    Ex x σ p -> do
      curx <- State.gets (Rename.lookup x)
      x' <- rename x
      p' <- rename p
      case curx of 
        Nothing -> State.modify (Rename.delete x)
        Just curx' -> State.modify (Rename.insert x curx')
      return $ Ex x' σ p'
    Diamond w p -> do
      w' <- rename w
      p' <- rename p
      return $ Diamond w' p'

instance Rename Neg where
  rename f = case f of
    NAtom a -> rename a >>= return . NAtom
    And p q -> do
      p' <- rename p
      q' <- rename q
      return $ And p' q'
    Top -> return Top
    Imp p q -> do
      p' <- rename p
      q' <- rename q
      return $ Imp p' q'
    Iff p q -> do
      p' <- rename p
      q' <- rename q
      return $ Iff p' q'
    Up a -> rename a >>= return . Up
    Box w p -> do
      w' <- rename w
      p' <- rename p
      return $ Box w' p'
    All x σ p -> do
      curx <- State.gets (Rename.lookup x)
      x' <- rename x
      p' <- rename p
      case curx of 
        Nothing -> State.modify (Rename.delete x)
        Just curx' -> State.modify (Rename.insert x curx')
      return $ All x' σ p'

instance Petrify Pos where
  petrify = onAtomsP . petrify

instance Petrify Neg where
  petrify = onAtomsN . petrify

instance Apply Neg where
  f ✴ θ = onAtomsN (✴ θ) f

instance Apply Pos where
  f ✴ θ = onAtomsP (✴ θ) f

-- @ Printing

instance Print Pos where
  pPrint = printPos pPrint 0

instance Print Neg where
  pPrint = printNeg pPrint 0

andPrec, orPrec, impPrec, iffPrec, arrowPrec, quantPrec, boxPrec :: Int
boxPrec = 10
arrowPrec = 9
andPrec = 8
orPrec = 7
impPrec = 6
iffPrec = 5
quantPrec = 2

{- 
Abstract over the atom printer to account for different atom printers
wrt sorts.
-} 

printNeg :: (Atom -> PP.Doc) -> Int -> Neg -> PP.Doc
printNeg afn pr f = case f of
  NAtom a -> afn a
  And p q -> PP.paren (pr > andPrec) pq 
    where pq = PP.sep [PP.hsep[p', PP.text "∧"],
                       q']
          p' = printNeg afn (andPrec+1) p
          q' = printNeg afn andPrec q
  Top -> PP.text "⊤"
  Imp p q -> PP.paren (pr > impPrec) pq 
    where pq = PP.sep [PP.hsep[p', PP.text "⊃"],
                       q']
          p' = printPos afn (impPrec+1) p
          q' = printNeg afn impPrec q
  Iff p q -> PP.paren (pr > iffPrec) pq 
    where pq = PP.sep [PP.hsep[p', PP.text "⇔"],
                       q']
          p' = printNeg afn (iffPrec+1) p
          q' = printNeg afn iffPrec q
  All x σ p -> PP.paren (pr > quantPrec) q
    where q = PP.sep [ PP.hsep [ PP.text "∀"
                               , PP.hcat[ PP.text "(", pPrint x, PP.text " : ", pPrint σ
                                        , PP.text ")."]]
                     , p']
          p' = printNeg afn quantPrec p
  Up p -> PP.paren (pr > arrowPrec) (PP.sep[PP.text "↑", p'])
    where p' = printPos afn arrowPrec p
  Box w p -> w' <> p'
    where w' = if w == Term.defaultPrincipal then PP.empty else pPrint w <> PP.space
          p' = PP.text "□" <+> printNeg afn boxPrec p

printPos :: (Atom -> PP.Doc) -> Int -> Pos -> PP.Doc
printPos afn pr f = case f of
  PAtom a -> afn a
  Tensor p q -> PP.paren (pr > andPrec) pq 
    where pq = PP.sep [PP.hsep[p', PP.text "⊗"],
                       q']
          p' = printPos afn (andPrec+1) p
          q' = printPos afn andPrec q
  One -> PP.text "1"
  Or p q -> PP.paren (pr > orPrec) pq 
    where pq = PP.sep [PP.hsep[p', PP.text "∨"],
                       q']
          p' = printPos afn (orPrec+1) p
          q' = printPos afn orPrec q
  Bot -> PP.text "⊥"
  Ex x σ p -> PP.paren (pr > quantPrec) q
    where q = PP.sep [ PP.hsep[ PP.text "∃"
                              , PP.hcat [ PP.text "(", pPrint x, PP.text " : ", pPrint σ
                                        , PP.text ")."]]
                     , p']
          p' = printPos afn quantPrec p
  Down p -> PP.paren (pr > arrowPrec) (PP.sep[PP.text "↓", p'])
    where p' = printNeg afn arrowPrec p
  Diamond w p -> w' <> p'
    where w' = if w == Term.defaultPrincipal then PP.empty else pPrint w <> PP.space
          p' = PP.text "◇" <+> printPos afn boxPrec p
