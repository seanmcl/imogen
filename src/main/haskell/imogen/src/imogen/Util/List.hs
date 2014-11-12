
-- | List utilities.

-- @ Signature 

module Imogen.Util.List 
  ( module Data.List 
  , allInjectiveMaps
  , findRem
  , findFirst
  , findRemFirst
  , partitions
  , gpartitions
  , select
  , sublists
  , splits
  , classify
  , partitionMaybe
  , partitionEither
  , uncons
  , foldr2
  , allCombs
  )
where

-- @ Imports

import Imogen.Util.Prelude 
import Data.List

-- @ List utils

-- allCombs [[1,2], [3, 4], [5]] = [ [1,3,5], [1,4,5], [2,3,5], [2,4,5] ]

allCombs :: [[a]] -> [[a]]
allCombs [] = [[]]
allCombs (c:cs) = do x <- c
                     xs <- allCombs cs
                     return $ x : xs

{- 
| Given two lists l1 l2 where |l1| >= |l2|, return all
injective maps between the two lists.  For example
allInjectiveMaps [1,2,3] [4,5,6] = [[(1,4), (2,5), (3,6)], [(1,5),(2,4),(3,6)], ...]
-} 

allInjectiveMaps :: [a] -> [b] -> [[(a,b)]]
allInjectiveMaps [] _ = [[]]
allInjectiveMaps (_:_) [] = []
allInjectiveMaps (x:xs) ys = 
  let sels = map (flip select ys) [0..length ys-1] 
      f (y, ys') = map ((x, y):) (allInjectiveMaps xs ys')
  in concatMap f sels

-- Grab the nth element out of a list and return the element with
-- the modified list.

select :: Int -> [a] -> (a, [a])
select _ [] = error "select: empty" 
select 0 (x:xs) = (x, xs)
select n (x:xs) = (y, x:xs')
  where (y, xs') = select (n-1) xs

-- | Return the first element satisfying the function.

findFirst :: (a -> Maybe b) -> [a] -> Maybe b
findFirst _ [] = Nothing
findFirst f (x:xs) = case f x of
                       Nothing -> findFirst f xs
                       Just y -> Just y

-- | Return the first element satisfying the function, and removing
--   it from the list.

findRemFirst :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
findRemFirst _ [] = Nothing
findRemFirst f (x:xs) = case f x of
  Nothing -> case findRemFirst f xs of
    Nothing -> Nothing
    Just (v, xs') -> Just (v, x:xs')
  Just y -> Just (y, xs)

findRem :: (a -> Bool) -> [a] -> Maybe (a, [a])
findRem p = findRemFirst (\x -> if p x then Just x else Nothing)

-- | Return all partitions of a list

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = concatMap f parts
  where parts = partitions xs
        f p = ([x]:p) : insertAll x p

-- | Guarded partition

gpartitions :: ([[a]] -> Bool) -> [a] -> [[[a]]]
gpartitions _ [] = [[]]
gpartitions f (x:xs) = 
  let parts = gpartitions f xs
      fn p = 
        let ps = ([x]:p) : insertAll x p in
        filter f ps
  in concatMap fn parts

{- 
| insertAll x [l1, ..., ln] --> [[(x:l1), l2, ..., ln], 
                                 [l1, (x:l2), ..., ln], 
                                 ...
                                 [l1, l2, ..., (x:ln)]]
-} 

insertAll :: a -> [[a]] -> [[[a]]]
insertAll _ [] = []
insertAll x (l:ls) = ((x:l) : ls) : map (l:) ls'
  where ls' = insertAll x ls

-- | Return all sublists of a list

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = xs' ++ map (x:) xs'
  where xs' = sublists xs

-- | Return all sublists of a list with the corresponding list difference

splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits (x:xs) = map (use x) xs' ++ map (leave x) xs'
  where xs' = splits xs
        use a (as, bs) = (a:as, bs)
        leave a (as, bs) = (as, a:bs)

classify :: (a -> Either b c) -> [a] -> ([b], [c])
classify _ [] = ([], [])
classify f (x:xs) = 
  case f x of 
    Left a -> (a:as, bs)
    Right b -> (as, b:bs)
  where (as, bs) = classify f xs

partitionMaybe :: (a -> Maybe (Either b c)) -> [a] -> ([b], [c])
partitionMaybe _ [] = ([], [])
partitionMaybe f (x:xs) =
  let yzs@(ys, zs) = partitionMaybe f xs in
  case f x of 
    Nothing -> yzs
    Just (Left y) -> (y:ys, zs)
    Just (Right z) -> (ys, z:zs)

partitionEither :: (a -> Either b c) -> [a] -> ([b], [c])
partitionEither f = partitionMaybe (Just . f)

uncons :: [a] -> (a, [a])
uncons [] = error "Impossible"
uncons (h:t) = (h, t)

foldr2 :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldr2 _ b [] [] = b
foldr2 f b (x:xs) (y:ys) = foldr2 f (f x y b) xs ys
foldr2 _ _ _ _ = error "foldr2: length mismatch"
