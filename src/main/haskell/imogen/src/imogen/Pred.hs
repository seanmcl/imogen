
-- | Predicate labels 

-- @ Pragmas

{-# LANGUAGE CPP, EmptyDataDecls, DeriveDataTypeable #-} 

-- @ Signature

module Imogen.Pred
  ( -- * Predicates
    Pred
  , Preds(..)
    -- * Util
  , make
  , name
    -- * Classifiers
  , isPos
  , isNeg
  , isConstr
    -- * Fresh prediate names
  , start
  , next
  , next'
  )
where 

-- @ Imports

#include "../undefined.h" 

import Imogen.Util.Prelude
import qualified Data.Char as Char
import qualified Data.Generics as G
import Data.Generics(Typeable(..))
import qualified Imogen.Util.Name as Name
import Imogen.Util.Name (Name, HasName)
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)
import qualified Imogen.Util.Set as Set

-- @ Predicate labels

-- @@ Tag

data PredTag 

instance Typeable PredTag where
  typeOf _ = G.mkTyConApp (G.mkTyCon "Imogen.Pred.PredTag") []

-- @@ Type

-- | Predicate labels.  Note that we don't maintain the arity.  The signature
--   will tell us the arity.
newtype Pred = Pred (Name PredTag)
  deriving (Ord, Eq, Show, Data, Typeable)

instance HasName Pred where
  name (Pred n) = Name.name n

instance Print Pred where
  pPrint (Pred p) = pPrint p

-- @ Preds

-- | Grab all predicate labels from an object.
class Preds a where
  -- | The predicate labels in an object.
  preds :: a -> Set Pred
  preds = Set.map fst . arityPreds
  -- | The predicate labels with arity.  Used for sort inference.
  arityPreds :: a -> Set (Pred, Int)

-- @ Classifiers

-- @@ Positive

-- | Is the given predicate positive?
isPos :: Pred -> Bool
isPos (Pred n) = elem s posAtoms || Char.isLower (head s)
  where s = Name.name n 

posAtoms :: [String]
posAtoms = ["ⓦ", "ⓕ", "=", "<", ">", "≤", "≥" ]

-- @@ Negative

-- | Is the given predicate negative?
isNeg :: Pred -> Bool
isNeg (Pred n) = elem s negAtoms || Char.isUpper (head s)
  where s = Name.name n 

negAtoms :: [String]
negAtoms = ["◃"]

-- @@ Constraints

-- | Is the given predicate from a constraint domain?
isConstr :: Pred -> Bool
isConstr (Pred n) = elem s constrs
  where s = Name.name n 

constrs :: [String]
constrs = [ "=", "<", ">", "≤", "≥" ]

-- @ Util

{- | 
Create a predicate from a name.  Note that this should only be used
during parsing.  Normally you would create a fresh variable using
'next' or 'next''
-} 
make :: String -> Pred
make = Pred . Name.make

-- | The name portion of a variable.  (I.e., without the fresh index.)
name :: Pred -> String
name (Pred p) = Name.name p

-- @ Fresh names

-- | The starting point for fresh name generation.
start :: Pred
start = Pred $ Name.start "L"

-- | Generate the next fresh variable name from the given seed.
next :: String -> Pred -> Pred
next s (Pred x) = Pred $ Name.next s x

-- | Generate the next fresh variable name from the given seed and the current label.
next' :: Pred -> Pred
next' (Pred x) = Pred $ Name.next (Name.name x) x

