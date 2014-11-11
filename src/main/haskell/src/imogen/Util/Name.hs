
-- | Names

-- @ Pragmas

{-# LANGUAGE DeriveDataTypeable #-} 

-- @ Signature

module Imogen.Util.Name
  ( Name
  , start
  , make
  , unsafeMake
  , next
  , HasName(name)
  , cast
  , marshal
  , unmarshal
  )
where 

-- @ Imports

import Imogen.Util.Prelude
import qualified Control.Monad as M
import qualified Test.QuickCheck as Q
import qualified Data.List as List
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print)

-- @ Names

-- Use a phantom type to make different incomparable variables.

newtype Name a = Name (String, Integer)
  deriving (Ord, Eq, Show, Data, Typeable)

defaultIndex :: Integer
defaultIndex = -1

make :: String -> Name a
make α = Name (α, defaultIndex)

-- | Make sure you know what you are doing if you use this.

unsafeMake :: String -> Integer -> Name a
unsafeMake = curry Name

class HasName a where
  name :: a -> String

instance HasName (Name a) where
  name (Name (n, _)) = n

next :: String -> Name a -> Name a
next s (Name (_, n)) = Name (s, n+1)

start :: String -> Name a
start s = Name (s, 0)

-- Note, cast ≠ id

cast :: Name a -> Name b
cast (Name x) = Name x

{- 
marshal . unmarshal = id
unmarshal . marshal = id
-} 

marshal :: Name a -> String
marshal (Name (s, ind)) 
  | ind == defaultIndex = s
  | otherwise = s ++ "%" ++ show ind

unmarshal :: String -> Name a
unmarshal x = case List.elemIndex '%' x of
  Nothing -> Name (x, defaultIndex)
  Just k -> Name (take k x, read $ drop (k+1) x)

-- @ Printing

instance Print (Name a) where
  pPrint (Name (α, n)) 
    | n == defaultIndex = PP.text α 
    | otherwise = PP.text $ α ++ show n

-- @ QuickCheck

instance Q.Arbitrary (Name a) where
  arbitrary = M.liftM make (Q.elements ["x" ++ show n | n :: Int <- [1 .. 9]]) 

