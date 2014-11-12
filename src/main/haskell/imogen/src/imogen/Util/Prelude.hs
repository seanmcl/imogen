
-- @ Signature

module Imogen.Util.Prelude
  ( module Prelude
  , State
  , StateT
  , MonadState
  , Reader
  , ReaderT
  , MonadReader
  , Set
  , Map
  , Data
  , Typeable
  , (<$>)
  , first
  , second
  , both
  , Impossible(..)
  , throwImpossible
  , (<>), (<+>), ($$)
  , error'
  , Doc
  , pprint
  , assert
  , list
  )
where

-- @ Imports

-- Use System.IO.UTF8 for printing

import Prelude hiding (print, putStr, putStrLn)
import qualified Control.Applicative as App
import qualified Control.Arrow as Arr
import Control.Monad.State
import Control.Monad.Reader
import Data.Generics(Data, Typeable)
import Data.Map(Map)
import Data.Set(Set)
import qualified Imogen.Util.Debug as Debug
import Imogen.Util.Impossible
import qualified Imogen.Util.Print as Print
import Imogen.Util.Print (Doc, Print, pPrint, (<>), (<+>), ($$))

-- @ Prelude extras

list :: a -> [a]
list x = [x]

assert :: Bool -> a -> a
assert = Debug.assert

error' :: Doc -> a
error' = Debug.error

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = (App.<$>)

first :: (a -> b) -> (a, c) -> (b, c)
first = Arr.first

second :: (b -> c) -> (a, b) -> (a, c)
second = Arr.second

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

pprint :: Print a => a -> IO ()
pprint = Print.putStrLn . pPrint
