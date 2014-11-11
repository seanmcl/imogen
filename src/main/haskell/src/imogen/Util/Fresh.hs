
-- | A common interface for monads which allow some kind of fresh name
-- generation.

-- @ Pragmas

{-# LANGUAGE MultiParamTypeClasses #-}

-- @ Signature

module Imogen.Util.Fresh 
  ( HasFresh(..)
  , fresh
  ) 
where

-- @ Imports

import Imogen.Util.Prelude 
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R

-- @ Fresh names

-- Fresh variables of type i in state s

class HasFresh i s where
  nextFresh :: String -> s -> (i,s)

fresh :: (MonadState s m, HasFresh i s) => String -> m i
fresh name = do 
  (i, s) <- S.gets (nextFresh name)
  S.put s
  return i

