
-- @ Signature

module Imogen.Util.Seq 
  ( module Data.Sequence
  , ViewL2(..)
  , viewl2
  , ViewR2(..)
  , viewr2
  , toList
  )
where

-- @ Imports

import qualified Data.Foldable as Fold
import Data.Sequence

-- @ Util

data ViewL2 a = EmptyL2
              | SingL2 a
              | (a, a) ::< (Seq a)

viewl2 :: Seq a -> ViewL2 a
viewl2 q = case viewl q of 
  EmptyL -> EmptyL2
  x :< xs -> case viewl xs of
    EmptyL -> SingL2 x
    y :< ys -> (x, y) ::< ys

data ViewR2 a = EmptyR2
              | SingR2 a
              | Seq a ::> (a, a)

viewr2 :: Seq a -> ViewR2 a
viewr2 q = case viewr q of 
  EmptyR -> EmptyR2
  xs :> x -> case viewr xs of
    EmptyR -> SingR2 x
    ys :> y -> ys ::> (y, x)

toList :: Seq a -> [a]
toList = Fold.toList
