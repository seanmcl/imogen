
-- @ Pragmas

-- All pragmas are to support Monad m => Functor m

{-# LANGUAGE UndecidableInstances, FlexibleInstances, OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- @ Signature 

module Imogen.Util.Monad 
  ( module Control.Monad
  , module Control.Monad.Trans
  , FileReader(..)
  , puts
  , any
  , all
  , findM
  , ignore
  , ifM
  , ifM'
  , maybeM
  , mapMaybeM
  , concatMapM
  , lift2
  , lift3
  , foldrM
  , findRemM
  , findRemFirstM
  , partitionM
  ) 
where

-- @ Imports 

import Imogen.Util.Prelude hiding (any, all)
import qualified Control.Monad.State as State
import Control.Monad
import Control.Monad.Trans
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Fold
import qualified System.IO.UTF8 as Sys

-- @ FileReader

class Monad m => FileReader m where
  readFile :: FilePath -> m String

instance FileReader IO where
  readFile = Sys.readFile

-- @ Monad utils

instance Monad m => Functor m where
  fmap f x = x >>= return . f

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM _ [] = return []
concatMapM f (x:xs) = do
  ys <- f x
  ys' <- concatMapM f xs
  return $ ys ++ ys'

foldrM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldrM = Fold.foldrM

maybeM :: Monad m => b -> (a -> b) -> m (Maybe a) -> m b
maybeM b = fmap . maybe b

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = fmap Maybe.catMaybes . mapM f

puts :: (MonadState s m) => (s -> a -> s) -> a -> m ()
puts f x = do s <- State.get
              State.put (f s x)
              return ()

any :: Monad m => (a -> m Bool) -> [a] -> m Bool
any p xs = do bs <- mapM p xs
              return $ elem True bs

all :: Monad m => (a -> m Bool) -> [a] -> m Bool
all p xs = do bs <- mapM p xs
              return $ notElem False bs

ignore :: Monad m => m a -> m ()
ignore = (>> return ())

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM g x y = do b <- g
               if b then x else y

ifM' :: Monad m => m Bool -> a -> a -> m a
ifM' g x y = do b <- g
                if b then return x else return y

lift2 :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) => m a -> t1 (t2 m) a
lift2 = lift . lift 

lift3 :: (MonadTrans t1, MonadTrans t2, MonadTrans t3, Monad m, Monad (t3 m), Monad (t2 (t3 m))) => m a -> t1 (t2 (t3 m)) a
lift3 = lift . lift . lift

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _p [] = return Nothing
findM p (x:xs) = ifM (p x) (return $ Just x) (findM p xs)

findRemFirstM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe (b, [a]))
findRemFirstM _ [] = return Nothing
findRemFirstM p (x:xs) = do
  r <- p x
  case r of 
    Just y -> return $ Just (y, xs)
    Nothing -> do
      r' <- findRemFirstM p xs
      case r' of
        Nothing -> return Nothing
        Just (y, xs') -> return $ Just (y, x:xs')

findRemM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe (a, [a]))
findRemM p = findRemFirstM (\x -> p x >>= \r -> return $ if r then Just x else Nothing)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM p (x:xs) = do
  (yes, no) <- partitionM p xs
  ifM' (p x) (x:yes, no) (yes, x:no)

