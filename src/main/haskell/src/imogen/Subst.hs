
{- | 
/Substitutions/.

Substitutions are fundamental in Imogen.  There are two different kinds of substitutions.
There are /basic substitutions/, defined in this module, with type Θ.  Then there is
the module "Imogen.CSubst" for /constraint substitutions/.  

Basic substitutions represent the usual notion, mapping variables to terms.  
Unfortunately, not all unification algorithms in Imogen are unitary.  In most
cases, when a unification problem is non-unitary, we choose not to generate all
substitution instances, but instead maintain the unresolved equations as /constraints/.
A constraint substitution is (morally) a pair of a basic substitution (of type Θ) and 
a constraint (of type Constr.Ψ).  The type of constraint substitutions is also
called Θ.  

This module provides all the usual utility functions on substitutions.
-} 

-- @ Pragmas

{-# LANGUAGE CPP, MultiParamTypeClasses #-} 

-- @ Signature

module Imogen.Subst
  ( -- * Substitutions
    Θ
    -- * Application
  , lookup
  , GenApply(..)
  , Apply(..)
    -- * Constructors
  , ι 
  , (○)
  , (↦)
  , (⟼)
    -- * Conversions
  , map
  , fromList
  , toMap
  , toList
    -- * Domain\/Codomain
  , dom
  , cod
    -- * Misc
  , print
  )
where

-- @ Imports

#include "../undefined.h" 

import Imogen.Util.Prelude hiding (lookup, map)
import qualified Data.List as List
import qualified Data.Map as Map
import Imogen.Param (Freeze(..))
import Imogen.Term (Term(..))
import qualified Imogen.Util.Debug as Debug
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Util.Seq as Seq
import Imogen.Util.Seq (Seq)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∅))
import Imogen.Var (Var, Vars(vars))

-- @ Substitutions

-- | Substitutions Θ are (morally) a 'Map' from variables to terms.
data Θ = Θ { toMap :: Map Var Term }
  deriving (Show, Eq, Ord)

toList :: Θ -> [(Var, Term)]
toList = Map.toList . toMap

-- | The identity substitution. 

ι :: Θ
ι = Θ Map.empty

-- | Lookup a variable in the substitution.

lookup :: Var -> Θ -> Maybe Term
lookup x = Map.lookup x . toMap

-- | Domain

dom :: Θ -> Set Var
dom = Map.keysSet . toMap 

-- | Codomain

cod :: Θ -> Set Var
cod = Map.fold (\x b -> Set.union b $ vars x) (∅) . toMap

-- | Composition

(○) :: Θ -> Θ -> Θ
θ1 ○ θ2 = 
  if not (canCompose θ1 θ2) 
  then Debug.error $ PP.text "Can't compose:" <+> pPrint (θ1, θ2) 
  else Θ $ Map.union (toMap θ2) (toMap $ θ1 ✴ θ2)

canCompose :: Θ -> Θ -> Bool
canCompose θ1 θ2 = 
  Set.null (Set.intersection (dom θ1) (dom θ2)) &&
  Set.null (Set.intersection (dom θ1) (cod θ2))

-- | A singleton substitution.
(↦) :: Var -> Term -> Θ
x ↦ t = Θ $ Map.singleton x t

-- | Extend a substitution with a new binding.
(⟼) :: Var -> Term -> Θ -> Θ 
(x ⟼ t) θ = case lookup x θ of 
                Just t' | t == t' -> θ 
                        | otherwise -> __IMPOSSIBLE__
                Nothing -> Θ $ Map.insert x t (toMap θ)

-- | Build a substitution from a list.
fromList :: [(Var, Term)] -> Θ
fromList xts = foldr (\(x, t) -> x ⟼ t) ι xts 

-- | Map a Term transformation over the codomain of a substitution.
map :: (Term -> Term) -> Θ -> Θ
map f = Θ . Map.map f . toMap

instance Freeze Θ where
  freeze _ = __IMPOSSIBLE__ 
  thaw = map . thaw

-- @ Application

{- | 
Applying a substitution θ to a variable ('Var') x yields a 'Term', not a new 'Var'.
Usually though, a substitution maps an element of a type to another element of
the same type.  Thus there are two type classes.  One where the type changes
(used infrequently) and another where the type is the same (used frequently).
-} 
class GenApply a b where
  -- | Postfix substitution syntax.  (A 6-pointed star.)
  (✶) :: a -> Θ -> b 

instance GenApply a b => GenApply [a] [b] where
  l ✶ θ = List.map (✶ θ) l

-- | Substitution application.  See the comments for 'GenApply'
class Apply a where
  -- | Postfix substitution syntax.  (An 8-pointed star.)
  (✴) :: a -> Θ -> a 

-- @@ Instances

instance Apply a => Apply [a] where
  l ✴ θ = List.map (✴ θ) l

instance Apply a => Apply (Seq a) where
  l ✴ θ = fmap (✴ θ) l

instance Apply a => Apply (Maybe a) where
  x ✴ θ = fmap (✴ θ) x

instance (Ord a, Apply a) => Apply (Set a) where
  x ✴ θ = Set.map (✴ θ) x

instance (Apply a, Apply b) => Apply (a, b) where
  (a, b) ✴ θ = (a ✴ θ, b ✴ θ)

instance (Apply a, Apply b, Apply c) => Apply (a, b, c) where
  (a, b, c) ✴ θ = (a ✴ θ, b ✴ θ, c ✴ θ)

instance GenApply Var Term where
  α ✶ θ = case lookup α θ of
    Nothing -> Var α
    Just p -> p

instance Apply Term where
  Var x ✴ θ = x ✶ θ
  c@(Param _) ✴ _ = c
  Fn f ts ✴ θ = Fn f (ts ✴ θ)

instance Apply Θ where
  θ1 ✴ θ2 = Θ $ Map.map (✴ θ2) (toMap θ1)

instance Print Θ where
  pPrint = print pPrint

-- | Export the pretty printing function for use by "Imogen.CSubst"
print :: (Term -> PP.Doc) -> Θ -> PP.Doc
print tfn (Θ m) 
    | Map.null m = PP.text "id"
    | otherwise = PP.braces $ PP.cat $ join $ List.intersperse (PP.text ", ") docs 
    where docs = List.map f $ Map.toList m
          f (α, p) = PP.hsep [pPrint α, PP.text "↦", tfn p]
          join [] = []
          join [x] = [x]
          join (x:y:rest) = x <> y : join rest
