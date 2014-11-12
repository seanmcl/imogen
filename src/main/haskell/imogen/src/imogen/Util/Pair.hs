
-- @ Signature

module Imogen.Util.Pair 
  ( flip
  , first
  , second
  , map
  , copy
  )
where

-- @ Imports

import Prelude hiding (flip, map)
import qualified Control.Arrow as Arrow

-- @ Pair utils 

flip :: (a, b) -> (b, a)
flip (a, b) = (b, a) 

first :: (a -> c) -> (a, b) -> (c, b)
first = Arrow.first

second :: (b -> c) -> (a, b) -> (a, c)
second = Arrow.second

map :: (a -> b) -> (a, a) -> (b, b)
map f = first f . second f

copy :: a -> (a, a)
copy x = (x, x)

