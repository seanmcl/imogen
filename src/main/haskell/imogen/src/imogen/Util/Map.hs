
-- @ Signature

module Imogen.Util.Map 
  ( module Data.Map
  , mapM
  )
where

-- @ Imports

import Imogen.Util.Prelude hiding (mapM)
import Imogen.Util.Monad ()
import Data.Map
import qualified Data.Map as Map

-- @ Util

mapM :: (Ord a, Monad m) => (b -> m c) -> Map a b -> m (Map a c)
mapM f = Map.foldWithKey mfn (return Map.empty)
 where 
  mfn k x m = f x >>= \y -> fmap (Map.insert k y) m

