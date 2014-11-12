
-- | Variables

-- @ Pragmas

{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances, EmptyDataDecls, DeriveDataTypeable #-} 

-- @ Signature 

module Imogen.Var
  ( -- * Variables
    Var
  , Vars(..)
    -- * Util
  , make
  , name
    -- * Marshaling
  , marshal
  , unmarshal
    -- * Fresh variable names
  , start
  , next
    -- * Freezing
  , freeze
  , thaw
  ) 
where

-- @ Imports

#include "../undefined.h" 

import Imogen.Util.Prelude 
import Control.Monad.State as S
import qualified Data.Char as Char
import qualified Data.Foldable as Fold
import qualified Data.Generics as G
import Data.Generics (Typeable(..))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Imogen.Param (Param, Params(..))
import qualified Imogen.Param as Param
import qualified Imogen.Util.Fresh as Fresh
import Imogen.Util.Fresh (HasFresh)
import qualified Imogen.Util.Name as Name
import Imogen.Util.Name (Name)
import qualified Imogen.Util.Pair as Pair
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∅))
import Test.QuickCheck as Q

-- @ Variables

-- @@ Tag

-- | A tag for the phantom 'Name' type.
data VarTag 

instance Typeable VarTag where
  typeOf _ = G.mkTyConApp (G.mkTyCon "Imogen.Var.VarTag") []

-- @@ Type

-- | The type of variables.
newtype Var = Var (Name VarTag)
  deriving (Eq, Ord, Show, Data, Typeable)

instance Q.Arbitrary Var where
  arbitrary = liftM Var Q.arbitrary

instance HasFresh Var Var where
  nextFresh s = Pair.copy . next s

instance HasFresh Var (Var, a) where
  nextFresh s (x, y) = (i, (x', y))
    where (i, x') = Fresh.nextFresh s x

instance HasFresh Var (Var, a, b) where
  nextFresh s (x, y, z) = (i, (x', y, z))
    where (i, x') = Fresh.nextFresh s x

instance Print Var where
  pPrint (Var x) = pPrint x

-- @ Util

{- | 
Create a variable from a name.  Note that this should only be used
during parsing.  Normally you would create a fresh variable using
'next'
-} 
make :: String -> Var 
make = Var . Name.make

-- | The name portion of a variable.  (I.e., without the fresh index.)
name :: Var -> String
name (Var x) = Name.name x

-- @ Marshaling

{- |
Marshal a variable to a string for transport to another type.  For instance,
we use the ATP library to solve some constraints.  When translating formulas
from Imogen formulas to ATP formulas we use marshal and unmarshal to preserve
the original names.
 -} 
marshal :: Var -> String 
marshal (Var x) = Name.marshal x

-- | See documentation for 'marshal'
unmarshal :: String -> Var 
unmarshal = Var . Name.unmarshal

-- @ Fresh variable names

-- | The starting point for variable fresh name generation.
start :: String -> Var
start = Var . Name.start 

-- | Generate the next fresh variable name from the given seed.
next :: String -> Var -> Var
next s (Var x) = Var $ Name.next s x

-- @ Freezing

{- | 
When matching t1 and t2, we 'freeze' the variables of t2 into parameters
so they will not be instantiated.  We then need to 'thaw' them back to
variables.
-} 
freeze :: Var -> Param
freeze (Var x) = Param.unsafeMake $ Name.cast x

-- | See the documentation for 'freeze'.
thaw :: Param -> Var
thaw = Var . Name.cast . Param.unsafeName

-- @ Class for grabbing the (free) variables in an object.

{- | 
A class for grabbing the variables from an object.  When there are no
quantifiers in the type, 'free' == 'vars'.
-} 
class Vars a where
  -- | All variables
  vars :: a -> Set Var
  -- | Just the free variables
  free :: a -> Set Var
  -- | An object is ground if it has no free variables
  ground :: a -> Bool
  ground = Set.null . free 

-- @@ Instances 

instance Vars Var where
  vars x = Set.singleton x
  free = vars

instance Vars Param where
  vars _ = (∅)
  free = vars

instance Params Var where
  params _ = (∅)

instance Vars a => Vars [a] where
  vars = foldr Set.union (∅) . map vars
  free = foldr Set.union (∅) . map free

instance Vars a => Vars (Seq a) where
  vars = Fold.foldr Set.union (∅) . fmap vars
  free = Fold.foldr Set.union (∅) . fmap free

instance Vars a => Vars (Set a) where
  vars = Set.fold (Set.union . vars) (∅)
  free = Set.fold (Set.union . free) (∅)

instance (Vars a, Vars b) => Vars (Map b a) where
  vars = Map.foldWithKey (\k x s -> Set.unions [vars k, vars x, s]) (∅) 
  free = Map.foldWithKey (\k x s -> Set.unions [free k, free x, s]) (∅) 

instance (Vars a, Vars b) => Vars (a, b) where
  vars (x, y) = Set.union (vars x) (vars y)
  free (x, y) = Set.union (free x) (free y)

instance (Vars a, Vars b, Vars c) => Vars (a, b, c) where
  vars (x, y, z) = Set.unions [vars x, vars y, vars z]
  free (x, y, z) = Set.unions [free x, free y, free z]

instance (Vars a, Vars b, Vars c, Vars d) => Vars (a, b, c, d) where
  vars (x, y, z, w) = Set.unions [vars x, vars y, vars z, vars w]
  free (x, y, z, w) = Set.unions [free x, free y, free z, free w]
