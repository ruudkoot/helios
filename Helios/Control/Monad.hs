{-# LANGUAGE TupleSections #-}
module Helios.Control.Monad
( module Control.Monad
, iterateM
, iterateForM
, repeatM
, mapAccumM, mapAccumM_
, forAccumM, forAccumM_
) where

import Control.Monad
import Control.Monad.Extra

iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM k x = do
  y <- k x
  ys <- iterateM k y
  return (y:ys)

iterateForM :: Monad m => Int -> a -> (a -> m a) -> m [a]
iterateForM 0 _ _ = return []
iterateForM n x k = do
  y <- k x
  ys <- iterateForM (n-1) y k
  return (y:ys)

repeatM :: Monad m => m a -> m [a]
repeatM k = do
  x <- k
  xs <- repeatM k
  return (x:xs)

-- base-4.18
mapAccumM :: (Monad m) => (s -> a -> m (s, b)) -> s -> [a] -> m (s, [b])
mapAccumM _ s [] =
  return (s, [])
mapAccumM f s (x:xs) = do
  (s', y) <- f s x
  (s'', ys) <- mapAccumM f s' xs
  return (s'', y : ys)

mapAccumM_ :: (Monad m) => (s -> a -> m s) -> s -> [a] -> m s
mapAccumM_ f s xs
  = fst <$> mapAccumM (\s x -> (,()) <$> f s x) s xs

forAccumM :: (Monad m) => s -> [a] -> (s -> a -> m (s, b)) -> m (s, [b])
forAccumM s xs f
  = mapAccumM f s xs

forAccumM_ :: (Monad m) => s -> [a] -> (s -> a -> m s) -> m s
forAccumM_ s xs f
  = mapAccumM_ f s xs