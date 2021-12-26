{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Helios.Class.Space
( VectorSpace(..)
, MetricSpace(..)
, NormedVectorSpace(..)
, InnerProductSpace(..)
) where

infixl 6 |+|
infixl 6 |-|
infixl 7 .*
infixl 7 |.|

class Num f => VectorSpace v f where
  (|+|) :: v f -> v f -> v f
  (|-|) :: v f -> v f -> v f
  v |-| w = v |+| reflect w
  (.*) :: f -> v f -> v f
  reflect :: v f -> v f
  reflect v = (-1) .* v
  fromScalar :: f -> v f
  toScalar :: v f -> f
  isScalar :: v f -> Bool

-- FIXME: Should r be Floating?
-- FIXME: No instance for (MetricSpace Float (Vector Double))
class Floating r => MetricSpace r p where
  distance :: p -> p -> r

-- FIXME: Metic r (v f)
class (VectorSpace v r, MetricSpace r (v r)) => NormedVectorSpace v r where
  norm :: v r -> r
  direction :: v r -> v r
  direction v = (1 / norm v) .* v

class NormedVectorSpace v r => InnerProductSpace v r where
  (|.|) :: v r -> v r -> r
  -- FIXME: atan2?
  angle :: v r -> v r -> r
  angle x y = acos ((x |.| y) / (norm x * norm y))

{- UndecidableInstance

instance InnerProductSpace v r => Num (v r) where
  -- Vector
  fromInteger = fromScalar . fromInteger
  (+) = (|+|)
  (-) = (|-|)
  negate = reflect
  -- NormedVector
  abs = fromScalar . norm
  signum = direction
  -- InnerProduct
  x * y
    | isScalar x = toScalar x .* y
    | isScalar y = toScalar y .* x
    | otherwise = fromScalar (x |*| y)

instance InnerProductSpace v r => Fractional (v r) where
  fromRational = fromScalar . fromRational
  recip x
    | isScalar x = fromScalar (recip (toScalar x))
    | otherwise = errorNotScalar

-}
