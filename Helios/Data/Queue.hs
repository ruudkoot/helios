{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Helios.Data.Queue
( Queue(..)
, LIFO
, FIFO
) where

--------------------------------------------------------------------------------
-- * Queue
--------------------------------------------------------------------------------

class Queue f a where
  empty :: f a
  isEmpty :: f a -> Bool
  insert :: a -> f a -> f a
  insertAll :: [a] -> f a -> f a
  removeFirst :: f a -> (a, f a)

--------------------------------------------------------------------------------
-- * LIFO
--------------------------------------------------------------------------------

newtype LIFO a
  = LIFO [a]
  deriving (Eq, Ord, Show)

instance Queue LIFO a where
  empty
    = LIFO []
  isEmpty (LIFO xs)
    = null xs
  insert x (LIFO xs)
    = LIFO (x : xs)
  insertAll ys (LIFO xs)
    = LIFO (ys ++ xs)
  removeFirst (LIFO (x:xs))
    = (x, LIFO xs)

--------------------------------------------------------------------------------
-- * FIFO
--------------------------------------------------------------------------------

newtype FIFO a
  = FIFO [a]
  deriving (Eq, Ord, Show)

instance Queue FIFO a where
  empty
    = FIFO []
  isEmpty (FIFO xs)
    = null xs
  insert x (FIFO xs)
    = FIFO (xs ++ [x])
  insertAll ys (FIFO xs)
    = FIFO (xs ++ ys)
  removeFirst (FIFO (x:xs))
    = (x, FIFO xs)
