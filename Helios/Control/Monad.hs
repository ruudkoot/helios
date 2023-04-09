module Helios.Control.Monad
( module Control.Monad
, iterateM
, iterateForM
, repeatM
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
