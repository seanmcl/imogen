
{-# LANGUAGE CPP, FlexibleInstances, UndecidableInstances, OverlappingInstances #-} 

-- @ Signature

module Imogen.Util.Set 
  ( module Data.Set
  , (∅)
  , (∪)
  , (∩)
  , (∈)
  , (∉)
  , all
  , any
  , mapM
  , (⊂) 
  , (⊆) 
  )
where

-- @ Imports

#include "../../undefined.h" 

import Imogen.Util.Prelude hiding (all, any, mapM)
import Data.Set
import qualified Data.Set as Set

-- @ Util

(∅) :: Set a
(∅) = empty

(∪) :: Ord a => Set a -> Set a -> Set a
(∪) = union

(∩) :: Ord a => Set a -> Set a -> Set a
(∩) = intersection

(∈) :: Ord a => a -> Set a -> Bool
(∈) = member

(∉) :: Ord a => a -> Set a -> Bool
x ∉ s = not $ x ∈ s

(⊂) :: Ord a => Set a -> Set a -> Bool
(⊂) = Set.isProperSubsetOf

(⊆) :: Ord a => Set a -> Set a -> Bool
(⊆) = Set.isSubsetOf

all :: (a -> Bool) -> Set a -> Bool
all p = fold (\x b -> p x && b) True

any :: (a -> Bool) -> Set a -> Bool
any p = fold (\x b -> p x || b) False

mapM :: (Monad m, Ord b) => (a -> m b) -> Set a -> m (Set b)
mapM f = Set.fold foldfn (return Set.empty)
   where 
    foldfn x m = do
      x' <- f x 
      s <- m 
      return $ Set.insert x' s
