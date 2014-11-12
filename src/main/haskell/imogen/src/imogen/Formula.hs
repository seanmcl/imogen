
{- |
/Non-polarized formulas/.
-} 

-- @ Pragmas

{-# LANGUAGE CPP, Rank2Types, FlexibleContexts, DeriveDataTypeable, MultiParamTypeClasses #-} 

-- @ Signature

module Imogen.Formula
  ( -- * The constraint type
    Formula(..)
    -- * Syntax
  , (⊤), (⊥), (∧), (∨), (⊃), (¬), (∃), (¥)
    -- * Util
  , listAll
  , listEx
  , listAnd
  , listImp
  , destAnd
  , destImp
    -- * Transformations
  , simplify
  , nnf
  , dnf
  , cnf
  , pnf
  , miniscope
    -- * Translation
  , polarize
  , inferSig
    -- * Double negation
  , NN(..)
  , doubleNegate
    -- * Misc
  , print
  ) 
where

-- @ Imports

#include "../undefined.h"

import Imogen.Util.Prelude hiding (lookup)
import qualified ATP.Decidable as Decidable
import qualified ATP.FormulaSyn as F
import qualified ATP.Formula as F
import qualified ATP.Prop as Prop
import qualified ATP.Skolem as Skolem
import qualified Control.Monad.State as State
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Generics as G
import qualified Imogen.Atom as Atom
import Imogen.Atom (Atom(..))
import qualified Imogen.Func as Func
import qualified Imogen.Param as Param
import Imogen.Param (Params(..), Freeze(..), Petrify(..))
import qualified Imogen.Pred as Pred
import qualified Imogen.PFormula as F'
import Imogen.PFormula (Pos, Neg)
import qualified Imogen.Rename as Rename
import Imogen.Rename (Rename(rename))
import qualified Imogen.Sig as Sig
import Imogen.Sig (Σ)
import qualified Imogen.Sort as Sort
import Imogen.Sort (Base)
import Imogen.Subst (Apply, (✴))
import Imogen.Term (Term(..))
import qualified Imogen.Util.List as List
import qualified Imogen.Util.Monad as M
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∅), (∪))
import qualified Imogen.Var as Var
import Imogen.Var (Var, Vars(..))

-- @ Formulas

data Formula = Atom Atom             -- ^ Atomic formulas
             | Top                   -- ^ ⊤
             | Bot                   -- ^ ⊥
             | Not Formula           -- ^ ¬ p
             | And Formula Formula   -- ^ p ∧ q
             | Or Formula Formula    -- ^ p ∨ q
             | Imp Formula Formula   -- ^ p ⊃ q
             | Iff Formula Formula   -- ^ p ⇔ q
             | All Var Base Formula  -- ∀ X : σ. p
             | Ex Var Base Formula   -- ∃ X : σ. p
  deriving (Eq, Ord, Show, Data, Typeable)

-- @ Syntax

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

(⇔) :: Formula -> Formula -> Formula
(⇔) = Iff

(∃) :: Var -> Base -> Formula -> Formula
(∃) = Ex

(¥) :: Var -> Base -> Formula -> Formula
(¥) = All

-- @ Traversal

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

-- @ Instances

instance Pred.Preds Formula where
  arityPreds = overAtoms (\a s -> Pred.arityPreds a ∪ s) Set.empty

instance Func.Funcs Formula where
  arityFuncs = overAtoms (\a s -> Func.arityFuncs a ∪ s) Set.empty

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

-- @ Util

-- | > listEx [X, Y, Z] [σ1, σ2, σ3] p == ∃ (X : σ1) (Y : σ2) (Z : σ3). p
listEx :: [Var] -> [Base] -> Formula -> Formula
listEx xs σs p = List.foldr2 Ex p xs σs

-- | > listAll [X, Y, Z] [σ1, σ2, σ3] p == ∃ (X : σ1) (Y : σ2) (Z : σ3). p
listAll :: [Var] -> [Base] -> Formula -> Formula
listAll xs σs p = List.foldr2 All p xs σs

-- | > listAnd [a, b, c, d] = a ∧ b ∧ c ∧ d 
listAnd :: [Formula] -> Formula
listAnd [] = Top
listAnd [p] = p
listAnd ps = foldr1 And ps

-- | > listImp [a, b, c]  d = a ⊃ b ⊃ c ⊃ d
listImp :: [Formula] -> Formula -> Formula
listImp ps q = foldr (⊃) q ps

-- | > destAnd a ∧ (b ∧ c) ∧ d == [a, b, c, d]
destAnd :: Formula -> [Formula]
destAnd (And p q) = destAnd p ++ destAnd q
destAnd p = [p]

-- | > destImp a ⊃ b ⊃ c == ([a, b], c)
destImp :: Formula -> ([Formula], Formula)
destImp (Imp p q) = (p:ps, q')
  where (ps, q') = destImp q
destImp p = ([], p)

-- @ Marshalling to and from ATP Formulas

{- 
To marshal from formulas to ATP formulas, we need to remember which
ATP variables correspond to Imogen parameters and constants.  For
unmarshaling, we need to associate Imogen sorts with the ATP formulas.
-} 

data Env = Env { envParams :: Set F.Var
               , consts :: Set F.Var
               , sorts :: Map F.Var Base
               }
  deriving (Eq, Ord, Show)

empty :: Env
empty = Env (∅) (∅) Map.empty

addParam :: F.Var -> State Env ()
addParam x = do 
  env <- State.get
  State.put $ env { envParams = Set.insert x (envParams env) }

addConst :: F.Var -> State Env ()
addConst x = do 
  env <- State.get
  State.put $ env { consts = Set.insert x (consts env) }

isParam :: F.Var -> State Env Bool
isParam x = do 
  env <- State.get
  return $ Set.member x (envParams env)

isConst :: F.Var -> State Env Bool
isConst x = do 
  env <- State.get
  return $ Set.member x (consts env)

addSort :: String -> Base -> State Env ()
addSort x σ = do 
  env <- State.get
  let σs = sorts env
  case Map.lookup x σs of
    Nothing -> State.put $ env { sorts = Map.insert x σ σs }
    Just σ' | σ == σ' -> return ()
            | otherwise -> __IMPOSSIBLE__

class Marshal a b where
  marshal :: a -> State Env b
  unmarshal :: b -> State Env a

instance Marshal a b => Marshal [a] [b] where
  marshal = mapM marshal
  unmarshal = mapM unmarshal

-- Parameters become universally quantified variables.

instance Marshal Term F.Term where
  marshal t = case t of
    Var x -> return $ F.Var $ Var.marshal x 
    Param c -> do
      let c' = Param.marshal c 
      addParam c'
      return $ F.Var c'
    Fn n [] -> 
      let n' = Func.name n in
      if List.all Char.isDigit n' then return $ F.Fn n' [] 
      else do 
        let c = Func.marshal n
        addConst c
        return $ F.Var c
    _ -> error' $ PP.text "The DLO theory doesn't recognize the following term that arose during search:" 
                  $$ pPrint t
  unmarshal t = case t of 
    F.Var x -> do
      p1 <- isParam x
      p2 <- isConst x
      return $ 
       if p1 then Param $ Param.unmarshal x else
       if p2 then Fn (Func.unmarshal x) [] else 
       Var $ Var.unmarshal x
    F.Num n -> return $ Fn (Func.make $ show n) []
    F.Fn x τs 
      | τs == [] && all (\c -> Char.isDigit c || c == '-') x -> 
        return $ Fn (Func.make x) []
      | otherwise -> __IMPOSSIBLE__

instance Marshal Atom F.Rel where
  marshal a = case a of 
      Rel op [t1, t2] -> m (Pred.name op) [t1, t2]
      _ -> __IMPOSSIBLE__ 
    where m f ts = marshal ts >>= return . F.R f 
  unmarshal a = case a of 
      F.R p [t1, t2] -> do
        t1' <- unmarshal t1
        t2' <- unmarshal t2
        return $ Rel (Pred.make p) [t1', t2']
      _ -> __IMPOSSIBLE__

instance Marshal Formula F.Formula where
  marshal f = case f of
      Atom a -> marshal a >>= return . F.Atom
      Top -> return F.Top
      Bot -> return F.Bot
      Not p -> marshal p >>= return . F.Not
      And p q -> binop p q F.And
      Or p q -> binop p q F.Or
      Imp p q -> binop p q F.Imp
      Iff p q -> binop p q F.Iff
      All x σ p -> exop x σ p F.All
      Ex x σ p -> exop x σ p F.Ex
    where binop p q op = do
            p' <- marshal p
            q' <- marshal q
            return $ op p' q'
          exop x σ p op = do
            let x' = Var.marshal x
            addSort x' σ 
            p' <- marshal p
            return $ op x' p'
  unmarshal f = case f of 
      F.Atom a -> unmarshal a >>= return . Atom
      F.Top -> return Top
      F.Bot -> return Bot
      F.Not p -> unmarshal p >>= return . Not
      F.And p q -> binop p q And 
      F.Or p q -> binop p q Or
      F.Imp p q -> binop p q Imp 
      F.Iff p q -> binop p q Iff
      F.All x p -> exop x p All
      F.Ex x p -> exop x p Ex
    where binop p q op = do
            p' <- unmarshal p
            q' <- unmarshal q
            return $ op p' q'
          exop x p op = do
            let x' = Var.unmarshal x
            p' <- unmarshal p
            return $ op x' Sort.Real p'

toATP :: Formula -> (F.Formula, Env)
toATP f = 
  let (f', env) = State.runState (marshal f) empty
      ρs = Set.toList $ envParams env
  in (F.listAll ρs f', env)

fromATP :: Env -> F.Formula -> Formula
fromATP env f = State.evalState (unmarshal f) env

call :: (F.Formula -> F.Formula) -> Formula -> Formula
call conv ψ = fromATP env $ conv ψ'
  where (ψ', env) = toATP ψ

-- @ Transformations

simplify :: Formula -> Formula
simplify = call Skolem.simplify

nnf :: Formula -> Formula
nnf = call Skolem.nnf

pnf :: Formula -> Formula
pnf = call Skolem.pnf

cnf :: Formula -> Formula
cnf = call Prop.cnf

dnf :: Formula -> Formula
dnf = call Prop.dnf

miniscope :: Formula -> Formula
miniscope = call Decidable.miniscope

-- @ Translation

-- | Add shifts to polarize a formula.
polarize :: Formula -> Neg
polarize = polarizeN

polarizeN :: Formula -> Neg
polarizeN f = case f of 
  Atom a -> if Atom.isPos a then F'.Up $ F'.PAtom a else F'.NAtom a
  Top -> F'.Top
  And p q -> F'.And (polarizeN p) (polarizeN q)
  Not p -> polarizeN $ p ⊃ (⊥)
  Imp p q -> F'.Imp (polarizeP p) (polarizeN q)
  Iff p q -> F'.Iff (polarizeN p) (polarizeN q)
  All x s p -> F'.All x s (polarizeN p)
  _ -> F'.Up (polarizeP f)

polarizeP :: Formula -> Pos
polarizeP f = case f of
  Atom a -> if Atom.isPos a then F'.PAtom a else F'.Down $ F'.NAtom a
  And p q -> F'.Tensor (polarizeP p) (polarizeP q)
  Top -> F'.One
  Or p q -> F'.Or (polarizeP p) (polarizeP q)
  Bot -> F'.Bot
  Ex x s p -> F'.Ex x s (polarizeP p)
  _ -> F'.Down (polarizeN f)

-- | Infer a signature.
inferSig :: Formula -> Σ 
inferSig form = 
  let fs = Set.toList $ Func.arityFuncs form
      fs' = map (\(f, n) -> (Sig.Func f, Sort.Fun (replicate n Sort.U) Sort.U)) fs 
      ps = Set.toList $ Pred.arityPreds form
      ps' = map (\(p, n) -> (Sig.Pred p, Sort.Rel (replicate n Sort.U))) ps 
  in Sig.fromList $ fs' ++ ps'

-- @ Double negation

{- 
Godel-Genzten

    * If φ is atomic, then φN is ¬¬φ
    * (φ ∧ θ)N is φN ∧ θN
    * (φ ∨ θ)N is ¬(¬φN ∧ ¬θN)
    * (φ → θ)N is φN → θN
    * (¬φ)N is ¬φN
    * (∀x φ)N is ∀x φN
    * (∃x φ)N is ¬∀x ¬φN

Avigad-Glivenko-Orevkov 
-} 

data NN = GG -- ^ Godel-Gentzen
        | AGO -- ^ Avigad-Glivenko-Orevkov

doubleNegate :: NN -> Formula -> Formula
doubleNegate nn = case nn of 
  GG -> gg 
  AGO -> ago

 where 

  ago f = (¬) $ nnf ((¬) f)

  gg f = case f of
    Atom _ -> (¬) $ (¬) f
    And p q -> gg p ∧ gg q
    Or p q -> (¬) ((¬) (gg p) ∧ (¬) (gg q))
    Imp p q -> gg p ⊃ gg q
    Not p -> (¬) (gg p)
    All x σ p -> (¥) x σ (gg p)
    Ex x σ p -> (¬) $ (¥) x σ ((¬) (gg p))
    Iff p q -> gg p ⇔ gg q
    Top -> (⊤)
    Bot -> (⊥)

-- @ Printing

instance Print Formula where
  pPrint = print pPrint

notPrec, andPrec, orPrec, impPrec, iffPrec, quantPrec :: Int
notPrec = 9
andPrec = 8
orPrec = 7
impPrec = 6
iffPrec = 5
quantPrec = 2

print :: (Atom -> PP.Doc) -> Formula -> PP.Doc
print afn = print' 0
 where 
  print' pr f = case f of 
    Atom a -> afn a
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
      where f' = PP.hang (PP.text "∀" <+> PP.parens (pPrint x <+> PP.text ":" <+> pPrint σ) <> PP.text ". ")
                         2 p'
            p' = print' quantPrec p
    Ex x σ p -> PP.paren (pr > quantPrec) f'
      where f' = PP.hang (PP.text "∃" <+> PP.parens (pPrint x <+> PP.text ":" <+> pPrint σ) <> PP.text ". ")
                         2 p'
            p' = print' quantPrec p
