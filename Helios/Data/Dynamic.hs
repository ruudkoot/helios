{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Helios.Data.Dynamic
( module Data.Proxy
, module Data.Typeable
, Dynamic(..)
, Packet(..)
, Row(..)
, DynamicFunction(..)
, Typecase(..)
, typecase
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

instance Eq Packet where
  Packet p == Packet q
    | Just q' <- cast q
      = p == q'
    | otherwise
      = errorPacketTypeMismatch

instance Ord Packet where
  compare (Packet p) (Packet q)
    | Just q' <- cast q
      = compare p q'
    | otherwise
      = errorPacketTypeMismatch

instance Show Packet where
  show (Packet x) = "«" ++ show x ++ ":" ++ show (typeOf x) ++ "»"

--------------------------------------------------------------------------------
-- Row
--------------------------------------------------------------------------------

class Row r where
  rowWidth :: r -> Int
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

instance (Dynamic t) => Row [t] where
  rowWidth xs = length xs
  rowIndex n xs = Packet (xs !! n)

--------------------------------------------------------------------------------
-- Dynamic function
--------------------------------------------------------------------------------

class DynamicFunction r a where
  applyDynamicFunction :: a -> [Packet] -> r

instance
    DynamicFunction r r
  where
    applyDynamicFunction f []
      = f

instance
    ( Dynamic t1
    , Dynamic r
    )
    =>
    DynamicFunction r (t1 -> r)
  where
    applyDynamicFunction f ps@[Packet x1]
      | Just x1' <- cast x1
        = f x1'
      | otherwise
        = errorDynamicArguments (typeOf f) ps

instance
    ( Dynamic t1
    , Dynamic t2
    , Dynamic r
    )
    =>
    DynamicFunction r (t1 -> t2 -> r)
  where
    applyDynamicFunction f ps@[Packet x1, Packet x2]
      | Just x1' <- cast x1
      , Just x2' <- cast x2
        = f x1' x2'
      | otherwise
        = errorDynamicArguments (typeOf f) ps

instance
    ( Dynamic t1
    , Dynamic t2
    , Dynamic t3
    , Dynamic r
    )
    =>
    DynamicFunction r (t1 -> t2 -> t3 -> r)
  where
    applyDynamicFunction f ps@[Packet x1, Packet x2, Packet x3]
      | Just x1' <- cast x1
      , Just x2' <- cast x2
      , Just x3' <- cast x3
        = f x1' x2' x3'
      | otherwise
        = errorDynamicArguments (typeOf f) ps

instance
    ( Dynamic t1
    , Dynamic t2
    , Dynamic t3
    , Dynamic t4
    , Dynamic r
    )
    =>
    DynamicFunction r (t1 -> t2 -> t3 -> t4 -> r)
  where
    applyDynamicFunction f ps@[Packet x1, Packet x2, Packet x3, Packet x4]
      | Just x1' <- cast x1
      , Just x2' <- cast x2
      , Just x3' <- cast x3
      , Just x4' <- cast x4
        = f x1' x2' x3' x4'
      | otherwise
        = errorDynamicArguments (typeOf f) ps

instance
    ( Dynamic t1
    , Dynamic t2
    , Dynamic t3
    , Dynamic t4
    , Dynamic t5
    , Dynamic r
    )
    =>
    DynamicFunction r (t1 -> t2 -> t3 -> t4 -> t5 -> r)
  where
    applyDynamicFunction f ps@[Packet x1, Packet x2, Packet x3, Packet x4, Packet x5]
      | Just x1' <- cast x1
      , Just x2' <- cast x2
      , Just x3' <- cast x3
      , Just x4' <- cast x4
      , Just x5' <- cast x5
        = f x1' x2' x3' x4' x5'
      | otherwise
        = errorDynamicArguments (typeOf f) ps

--------------------------------------------------------------------------------
-- Type case
--------------------------------------------------------------------------------

data Typecase
  = Int
  | Integer
  | Double
  | Unknown

typecase :: Dynamic a => a -> Typecase
typecase x
  = case show (typeOf x) of
      "Int" ->
        Int
      "Integer" ->
        Integer
      "Double" ->
        Double
      _ ->
        Unknown

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

errorPacketTypeMismatch
  = error "Packet type mismatch"

errorDynamicArguments :: TypeRep -> [Packet] -> a
errorDynamicArguments tr ps
  = error $
      "applyDynamicFunction: " ++ show tr ++ " does not unify with " ++ show ps
