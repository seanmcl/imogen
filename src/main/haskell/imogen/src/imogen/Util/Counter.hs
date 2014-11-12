
{- | 
A common interface for monads which allow some kind of fresh name
generation.
-} 

-- @ Pragmas

{-# LANGUAGE MultiParamTypeClasses #-} 

-- @ Signature

module Imogen.Util.Counter
  ( -- * Counters
    Counter
  , start
  , tickCtr
  , dummyCtr
    -- * A counter in a state monad
  , HasCounter(..)
  , tick
  ) 
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import qualified Imogen.Util.Print as PP
import Imogen.Util.Print (Print, pPrint)

-- @ Counters

-- | Use a phantom type to make non-comparable counters.
newtype Counter a = Counter Integer
  deriving (Eq, Ord, Show)

instance Print (Counter a) where
  pPrint ctr@(Counter n) 
    | ctr == dummyCtr = PP.text "%"
    | otherwise = PP.integer n

-- | The initial counter value.
start :: Counter a
start = Counter 0

-- | A dummy counter for testing parsing.
dummyCtr :: Counter a
dummyCtr = Counter (-1)

-- | Increment the counter.
tickCtr :: Counter a -> Counter a
tickCtr (Counter n) = Counter (n+1)

-- @ State monad

-- | An easy way to pass counters along in a state monad.
class HasCounter i s where
  nextCtr :: s -> (Counter i, s)

-- | Increment the counter of the state monad.
tick :: (MonadState s m, HasCounter i s) => m (Counter i)
tick = do
  (n, s) <- S.gets nextCtr
  S.put s
  return n

