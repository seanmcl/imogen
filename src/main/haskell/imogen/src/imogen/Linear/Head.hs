-- FIXME: DOC
-- | Heads

-- @ Pragmas 

{-# LANGUAGE CPP, MultiParamTypeClasses #-} 

-- @ Signature

module Imogen.Linear.Head
  ( Head(..)
  , Tag(..)
  , var              
  , param            
  , const
  , unify
  )
where

-- @ Imports

#include "../../undefined.h" 

import Imogen.Util.Prelude hiding (id, const)
import qualified Control.Monad.State as State
import qualified Data.List as List
import qualified Imogen.CSubst as CSubst
import qualified Imogen.Func as Func
import Imogen.Func (Func)
import qualified Imogen.Param as Param
import Imogen.Param(Param, Params, Freeze(..))
import Imogen.Rename(Rename(..))
import qualified Imogen.Subst as Subst
import qualified Imogen.Sort as Sort
import Imogen.Subst (Θ, Apply, (✴), ι, (○), (↦))
import qualified Imogen.Term as Term
import Imogen.Term(Term(..), Encode(..))
import qualified Imogen.Unif as Unif
import qualified Imogen.Util.Log as Log
import Imogen.Util.Log (Log)
import qualified Imogen.Util.Pair as Pair
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∅))
import qualified Imogen.Var as Var
import Imogen.Var(Var, Vars)

-- @ Heads

data Const = P Param
           | C Func
  deriving (Eq, Ord, Show)

data Head = HVar Var
          | HConst Const
  deriving (Eq, Ord, Show)

data Tag = Tag
instance Sort.Tag Tag where
instance Sort.TagOf Tag Head where

instance Encode Head where
  encode (HVar x) = Term.Var x
  encode (HConst (P x)) = Term.Param x
  encode (HConst (C x)) = Term.Fn x []
  decode (Term.Var x) = HVar x
  decode (Term.Param x) = HConst $ P x
  decode (Term.Fn c []) = HConst $ C c
  decode _ = __IMPOSSIBLE__

instance Sort.Normalize Term Tag where
  normalize _ t = t

instance Sort.OK Head Tag where
  ok _ _ = True

instance Sort.OK Term Tag where
  ok _ _ = True

instance Rename Head where
  rename (HVar v) = rename v >>= return . HVar
  rename a = return a

instance Vars Head where
  vars (HVar φ) = Set.singleton φ
  vars _ = (∅)
  free = Var.vars

instance Params Head where
  params (HConst (P φ)) = Set.singleton φ
  params _ = (∅)

instance Freeze Head where
  freeze h = case h of
    HVar x -> HConst $ P $ Var.freeze x
    HConst _ -> h
  thaw cs h = case h of
    HConst (P c) -> if Set.member c cs then HVar $ Var.thaw c else h
    HConst _ -> h
    HVar _ -> __IMPOSSIBLE__ 

instance Apply Head where
  c@(HConst _) ✴ _ = c
  x@(HVar φ) ✴ θ = case decode <$> Subst.lookup φ θ of
    Nothing -> x
    Just h -> h

var :: Var -> Head
var = HVar

param :: Param -> Head
param = HConst . P

const :: Func -> Head
const = HConst . C

-- @ Unification

{- 
Unification on heads is simple.  Either one of the heads is a
variable in which case the pair unifies, or else they are
constants.  If the constants are equal the unification succeeds.
If not, it fails.

Unification maintains a state of the current substitution, along
with the usual variable counters.
-} 

type Eqn = (Head, Head)

unify1, unify1' :: (MonadState s m, Log m) => Eqn -> m (Maybe Θ)
unify1' (HVar φ, f) = 
  let θ :: Θ 
      θ = φ ↦ encode f in
  return $ Just θ
unify1' eq@(_, HVar _) = unify1' $ Pair.flip eq
unify1' (ĉ, ĉ') | ĉ == ĉ' = return $ Just ι
                    | otherwise = return Nothing

unify1 eq = do
  res <- unify1' eq
  return res

unify, unify' :: (MonadState s m, Log m) => [Eqn] -> m (Maybe Θ)
unify' [] = return $ Just ι
unify' (e:es) = do
  res <- unify1 e
  case res of 
    Nothing -> return Nothing
    Just θ -> do
      res' <- unify' $ es ✴ θ
      return $ case res' of 
        Nothing -> Nothing
        Just θ' -> Just $ θ ○ θ'

unify eqs = do
  res <- unify' eqs
  return res

instance Unif.Unif Head Tag where
  unify _ = fmap (fmap CSubst.fromSubst) . unify
  match tag eqs = do
    fmap (thaw frozen) $ Unif.unify tag (map (second freeze) eqs)
    where frozen = Set.map Var.freeze (Var.vars eqs)

-- @ Printing

instance Print Const where
  pPrint (P x) = pPrint x
  pPrint (C x) = pPrint x

instance Print Head where
  pPrint (HVar φ) = pPrint φ
  pPrint (HConst ĉ) = pPrint ĉ 
