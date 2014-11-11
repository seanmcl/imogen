
-- | Function symbols

-- @ Pragmas

{-# LANGUAGE CPP, EmptyDataDecls, DeriveDataTypeable #-} 

-- @ Signature 

module Imogen.Func
  ( -- * Function symbols
    Func
  , Funcs(..)
    -- * Util
  , make
  , name
  , isPetrified
  , isIntegerLiteral
    -- * Fresh variable names
  , start
  , next
    -- * Marshaling
  , marshal
  , unmarshal
  ) 
where

-- @ Imports

#include "../undefined.h" 

import Imogen.Util.Prelude 
import qualified Data.Char as Char
import qualified Data.Foldable as Fold
import qualified Data.Generics as G
import Data.Generics (Typeable(..))
import qualified Data.List as List
import qualified Imogen.Util.Name as Name
import Imogen.Util.Name (Name)
import Imogen.Util.Print (Print, pPrint)
import Imogen.Util.Seq (Seq)
import qualified Imogen.Util.Set as Set
import Imogen.Util.Set ((∅))

-- @ Function symbols

-- @@ Tag

-- | A tag for the phantom 'Name' type.
data FuncTag

instance Typeable FuncTag where
  typeOf _ = G.mkTyConApp (G.mkTyCon "Imogen.Func.FuncTag") []

-- @@ Type

-- | The type of parameters.
newtype Func = Func (Name FuncTag)
  deriving (Eq, Ord, Show, Data, Typeable)

instance Print Func where
  pPrint (Func x) = pPrint x

-- @ Util

-- | Create a 'Func' from a 'String'.
make :: String -> Func
make = Func . Name.make

-- | The 'String' name of a 'Func'.
name :: Func -> String
name (Func x) = Name.name x

-- | A non-parseable tag to identify petrified parameters.
petrifyName :: String
petrifyName = "#c"

-- | Whether the function symbol is a petrified parameter.
isPetrified :: Func -> Bool
isPetrified (Func n) = Name.name n == petrifyName

{- |
Whether the function symbol is an integer literal.  This is used
by the arithmetic decision procedures.
-} 
isIntegerLiteral :: Func -> Bool
isIntegerLiteral = List.all Char.isDigit . name

-- @ Fresh variable names

-- | The starting point for variable fresh name generation.
start :: Func
start = Func $ Name.start petrifyName

-- | Generate the next fresh variable name from the given seed.
next :: Func -> Func
next (Func c) = Func $ Name.next petrifyName c

-- @ Marshaling

{- |
Marshal a variable to a string for transport to another type.  For instance,
we use the ATP library to solve some constraints.  When translating formulas
from Imogen formulas to ATP formulas we use marshal and unmarshal to preserve
the original names.
 -} 
marshal :: Func -> String 
marshal (Func x) = Name.marshal x

-- | See the documentation for 'marshal'
unmarshal :: String -> Func
unmarshal = Func . Name.unmarshal

-- @ Class for grabbing the function symbols in an object

-- | A class for grabbing the function symbols in an object. 
class Funcs a where
  -- | The function symbols in an object.
  funcs :: a -> Set Func
  funcs = Set.map fst . arityFuncs
  -- | The function symbols, with arity.  Used for sort inference.
  arityFuncs :: a -> Set (Func, Int)

instance Funcs a => Funcs [a] where
  arityFuncs = foldr Set.union (∅) . map arityFuncs

instance Funcs a => Funcs (Seq a) where
  arityFuncs = Fold.foldr Set.union (∅) . fmap arityFuncs
