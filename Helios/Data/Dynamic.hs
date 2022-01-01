{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Helios.Data.Dynamic
  ( module Data.Proxy
  , module Data.Typeable
  , Dynamic(..)
  , Packet(..)
  , Row(..)
  , Predicate(..)
  ) where

import Data.Maybe ( fromJust )
import Data.Proxy
import Data.Typeable

--------------------------------------------------------------------------------
-- Dynamic
--------------------------------------------------------------------------------

type Dynamic a = (Typeable a, Eq a, Ord a, Show a)

--------------------------------------------------------------------------------
-- Packet
--------------------------------------------------------------------------------

data Packet = forall a. Dynamic a => Packet a

instance Show Packet where
  show (Packet x) = "«" ++ show x ++ ":" ++ show (typeOf x) ++ "»"

--------------------------------------------------------------------------------
-- Row
--------------------------------------------------------------------------------

class Row r where
  rowWidth :: Proxy r -> Int
  rowIndex :: Int -> r -> Packet

instance (Dynamic t1, Dynamic t2) => Row (t1,t2) where
  rowWidth _ = 2
  rowIndex 0 (x,_) = Packet x
  rowIndex 1 (_,x) = Packet x

instance (Dynamic t1, Dynamic t2, Dynamic t3) => Row (t1,t2,t3) where
  rowWidth _ = 3
  rowIndex 0 (x,_,_) = Packet x
  rowIndex 1 (_,x,_) = Packet x
  rowIndex 2 (_,_,x) = Packet x

instance (Dynamic t1, Dynamic t2, Dynamic t3, Dynamic t4)
            => Row (t1,t2,t3,t4) where
  rowWidth _ = 4
  rowIndex 0 (x,_,_,_) = Packet x
  rowIndex 1 (_,x,_,_) = Packet x
  rowIndex 2 (_,_,x,_) = Packet x
  rowIndex 3 (_,_,_,x) = Packet x

instance (Dynamic t1, Dynamic t2, Dynamic t3, Dynamic t4, Dynamic t5)
            => Row (t1,t2,t3,t4,t5) where
  rowWidth _ = 5
  rowIndex 0 (x,_,_,_,_) = Packet x
  rowIndex 1 (_,x,_,_,_) = Packet x
  rowIndex 2 (_,_,x,_,_) = Packet x
  rowIndex 3 (_,_,_,x,_) = Packet x
  rowIndex 4 (_,_,_,_,x) = Packet x

--------------------------------------------------------------------------------
-- Predicate
--------------------------------------------------------------------------------

-- TODO: nice error message if 'unification' fails

class Predicate a where
  applyPredicate :: a -> [Packet] -> Bool

instance
    Predicate Bool
  where
    applyPredicate f []
      = f

instance
    ( Dynamic t1
    )
    =>
    Predicate (t1 -> Bool)
  where
    applyPredicate f [Packet x1]
      | Just x1' <- cast x1
        = f x1'

instance
    ( Dynamic t1
    , Dynamic t2
    )
    =>
    Predicate (t1 -> t2 -> Bool)
  where
    applyPredicate f [Packet x1, Packet x2]
      | Just x1' <- cast x1
      , Just x2' <- cast x2
        = f x1' x2'

instance
    ( Dynamic t1
    , Dynamic t2
    , Dynamic t3
    )
    =>
    Predicate (t1 -> t2 -> t3 -> Bool)
  where
    applyPredicate f [Packet x1, Packet x2, Packet x3]
      | Just x1' <- cast x1
      , Just x2' <- cast x2
      , Just x3' <- cast x3
        = f x1' x2' x3'

instance
    ( Dynamic t1
    , Dynamic t2
    , Dynamic t3
    , Dynamic t4
    )
    =>
    Predicate (t1 -> t2 -> t3 -> t4 -> Bool)
  where
    applyPredicate f [Packet x1, Packet x2, Packet x3, Packet x4]
      | Just x1' <- cast x1
      , Just x2' <- cast x2
      , Just x3' <- cast x3
      , Just x4' <- cast x4
        = f x1' x2' x3' x4'

instance
    ( Dynamic t1
    , Dynamic t2
    , Dynamic t3
    , Dynamic t4
    , Dynamic t5
    )
    =>
    Predicate (t1 -> t2 -> t3 -> t4 -> t5 -> Bool)
  where
    applyPredicate f [Packet x1, Packet x2, Packet x3, Packet x4, Packet x5]
      | Just x1' <- cast x1
      , Just x2' <- cast x2
      , Just x3' <- cast x3
      , Just x4' <- cast x4
      , Just x5' <- cast x5
        = f x1' x2' x3' x4' x5'
