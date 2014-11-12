
{- |
/Atomic formulas/

An atomic formula is essentially a predicate symbol with a list of term arguments.

Note that the infix predicate symbols are hard-coded.  The user cannot
add new infix predicate symbols.
-} 

-- @ Pragmas

{-# LANGUAGE CPP, DeriveDataTypeable #-} 

-- @ Signature

module Imogen.Atom 
  ( -- * Atomic formulas
    Atom(..)
    -- * Util
  , isPos
  , isNeg
  , isConstr
  )
where 

-- @ Imports

#include "../undefined.h"

import Imogen.Util.Prelude hiding (lookup, pred)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Imogen.Func as Func
import Imogen.Func (Funcs)
import qualified Imogen.Param as Param
import Imogen.Param (Params, Freeze(..), Petrify(..))
import qualified Imogen.Pred as Pred
import Imogen.Pred (Pred, Preds(..))
import Imogen.Rename (Rename(rename))
import Imogen.Subst (Apply(..))
import qualified Imogen.Term as Term
import Imogen.Term (Term)
import qualified Imogen.Util.Name as Name
import Imogen.Util.Name (HasName)
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Var as Var
import Imogen.Var (Vars)

-- @ Atoms

-- | Atomic formulas
data Atom = Rel { pred :: Pred    -- ^ Grab the predicate symbol.
                , args :: [Term]  -- ^ Grab the arguments. 
                } 
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Does the 'Atom' have a positive predicate label?
isPos :: Atom -> Bool
isPos = Pred.isPos . pred

-- | Does the 'Atom' have a negative predicate label?
isNeg :: Atom -> Bool
isNeg = Pred.isNeg . pred

-- | Does the 'Atom' have a predicate label corresponding to a constraint domain?
isConstr :: Atom -> Bool
isConstr = Pred.isConstr . pred

-- @@ Instances

instance HasName Atom where
  name = Name.name . pred

instance Vars Atom where
  vars = Var.vars . args
  free = Var.vars

instance Params Atom where
  params = Param.params . args

instance Funcs Atom where
  arityFuncs = Func.arityFuncs . args

instance Preds Atom where
  arityPreds (Rel p τs) = Set.singleton (p, length τs)

instance Rename Atom where
  rename (Rel p τs) = do
    τs' <- rename τs
    return $ Rel p τs'

instance Apply Atom where
  (Rel p τs) ✴ θ = Rel p (τs ✴ θ)

instance Freeze Atom where
  freeze (Rel p ts) = Rel p (freeze ts)
  thaw cs (Rel p ts) = Rel p (thaw cs ts)

instance Petrify Atom where
  petrify ρ (Rel p ts) = Rel p (petrify ρ ts)

instance Print Atom where
  pPrint (Rel p ts) = case ts of 
    [] -> pPrint p
    _ -> pPrint p <> PP.parens (PP.fcat $ List.intersperse (PP.text ", ") (map pPrint ts))

