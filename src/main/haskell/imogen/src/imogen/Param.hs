
-- | Parameters

-- @ Pragmas

{-# LANGUAGE CPP, EmptyDataDecls, DeriveDataTypeable #-} 

-- @ Signature 

module Imogen.Param
  ( -- * Parameters 
    Param
  , Params(..)
    -- * Util
  , make
  , name
    -- * Fresh variable names
  , start
  , next
    -- * Freezing
  , Freeze(..)
  , unsafeName
  , unsafeMake
    -- * Petrifying
  , Petrify(..)
    -- * Marshaling
  , marshal
  , unmarshal
  ) 
where

-- @ Imports

#include "../undefined.h" 

import Imogen.Util.Prelude 
import qualified Control.Monad as M
import qualified Data.Char as Char
import qualified Data.Foldable as Fold
import qualified Data.Generics as G
import Data.Generics (Typeable(..))
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Imogen.Func as Func
import Imogen.Func (Func)
import qualified Imogen.Util.Name as Name
import Imogen.Util.Name (Name)
import Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint, (<>))
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set (Set, (∅))
import qualified Test.QuickCheck as Q

-- @ Parameters

-- @@ Tag

-- | A tag for the phantom 'Name' type.
data ParamTag 

instance Typeable ParamTag where
  typeOf _ = G.mkTyConApp (G.mkTyCon "Imogen.Param.ParamTag") []

-- @@ Type

-- | The type of parameters.
newtype Param = Param (Name ParamTag)
  deriving (Eq, Ord, Show, Data, Typeable)

instance Q.Arbitrary Param where
  arbitrary = M.liftM Param Q.arbitrary

instance Print Param where
  pPrint (Param x) = pPrint x <> PP.text "♯"

-- @ Util

{- | 
Create a parameter from a name.  Note that this should only be used
during parsing.  Normally you would create a fresh parameter using
'next'
-} 
make :: String -> Param
make = Param . Name.make

-- | The name portion of a parameter.  (I.e., without the fresh index.)
name :: Param -> String
name (Param x) = Name.name x

-- @ Marshaling 

{- |
Marshal a variable to a string for transport to another type.  For instance,
we use the ATP library to solve some constraints.  When translating formulas
from Imogen formulas to ATP formulas we use marshal and unmarshal to preserve
the original names.
 -} 
marshal :: Param -> String 
marshal (Param x) = Name.marshal x

-- | See the documentation for 'marshal'
unmarshal :: String -> Param
unmarshal = Param . Name.unmarshal

-- @ Fresh variable names

-- | The starting point for variable fresh name generation.
start :: String -> Param
start = Param . Name.start 

-- | Generate the next fresh parameter name from the given seed.
next :: String -> Param -> Param
next s (Param x) = Param $ Name.next s x

-- @ Unsafe

{- | 
'unsafeName' and 'unsafeMake' are used to cast between parameters and variables during
the freeze and thaw operations.  These should only be used by "Imogen.Var".
-} 
unsafeName :: Param -> Name ParamTag
unsafeName (Param x) = x

-- | See documentation for 'unsafeName'.
unsafeMake :: Name ParamTag -> Param
unsafeMake = Param

-- @ Freezing 

{- 
Freezing is used to implement matching given a unification algorithm.
To match a with b, we 'freeze' all variables of b (turn them
temporarily into fresh parameters), run the unification algorithm, and
then 'thaw' the parameters back to variables in the resulting
substitution.
-} 

class Freeze a where
  freeze :: a -> a
  thaw :: Set Param -> a -> a

instance Freeze a => Freeze [a] where
  freeze = map freeze
  thaw ps = map (thaw ps)

instance (Ord a, Freeze a) => Freeze (Set a) where
  freeze = Set.map freeze
  thaw ps = Set.map (thaw ps)

instance Freeze a => Freeze (Seq a) where
  freeze = fmap freeze
  thaw ps = fmap (thaw ps)

instance (Freeze a, Freeze b) => Freeze (a, b) where
  freeze (x, y) = (freeze x, freeze y)
  thaw ps (x, y) = (thaw ps x, thaw ps y)

instance Freeze a => Freeze (Maybe a) where
  freeze = fmap freeze
  thaw ps = fmap (thaw ps)

-- @ Petrifying

{- | 
Petrifying converts parameters created during the first inversion
phase to constants (elements of 'Func').  When we implement proof
terms, we'll need to unpetrfiy the parameters to plug the actual
parameters into the proof term.
-} 
class Petrify a where
  petrify :: Map Param Func -> a -> a

-- @@ Instances

instance Petrify a => Petrify (Maybe a) where
  petrify ρ = fmap (petrify ρ)

instance Petrify a => Petrify [a] where
  petrify ρ = map (petrify ρ)

instance (Petrify a, Petrify b) => Petrify (a, b) where
  petrify ρ (x, y) = (petrify ρ x, petrify ρ y)

instance (Petrify a, Petrify b, Petrify c) => Petrify (a, b, c) where
  petrify ρ (x, y, z) = (petrify ρ x, petrify ρ y, petrify ρ z)

instance (Ord a, Petrify a) => Petrify (Set a) where
  petrify ρ = Set.map (petrify ρ)

-- @ Class for grabbing the parameters in an object.

-- | A class for grabbing the parameters in an object. 
class Params a where
  params :: a -> Set Param

-- @@ Instances

instance Params Param where
  params x = Set.singleton x

instance Params a => Params (Maybe a) where
  params Nothing = Set.empty
  params (Just x) = params x

instance Params a => Params [a] where
  params = foldr Set.union (∅) . map params

instance Params a => Params (Seq a) where
  params = Fold.foldr Set.union (∅) . fmap params

instance Params a => Params (Set a) where
  params = Set.fold (Set.union . params) (∅)

instance (Params a, Params b) => Params (Map b a) where
  params = Map.foldWithKey (\k x s -> Set.unions [params k, params x, s]) (∅) 

instance (Params a, Params b) => Params (a, b) where
  params (x, y) = Set.union (params x) (params y)

instance (Params a, Params b, Params c) => Params (a, b, c) where
  params (x, y, z) = Set.unions [params x, params y, params z]

instance (Params a, Params b, Params c, Params d) => Params (a, b, c, d) where
  params (x, y, z, w) = Set.unions [params x, params y, params z, params w]
