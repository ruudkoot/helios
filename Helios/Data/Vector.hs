{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Helios.Data.Vector
( module Space
, Vector(toArray)
, (><)
, vector
, zero
, ones
, unit
, fromList
, (!)
, component
, toList
, dim
, isUnit
, pivot
, componentWise
, scalarMultiplication
, dotProduct
, euclidianNorm
, crossProduct
, tripleProduct
, parallel, (|||)
, orthogonal, (-|-)
, orthogonalProjection
, errorDimNotEq
) where

import qualified Helios.Class.Space as Space
import           Helios.Class.Space
  hiding ( fromScalar, toScalar, isScalar )

import qualified Helios.Data.Array as Array
import           Helios.Data.Bool
import qualified Helios.Data.List as List
import           Helios.Data.String

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- FIXME: Unboxed array
newtype Vector a = Vector { toArray :: Array.Array Int a }
  deriving (Eq, Ord, Functor)

instance List.IsList (Vector a) where
  type Item (Vector a) = a
  fromList = fromList
  toList = toList

instance Show a => Show (Vector a) where
  show = showVector

showVector :: (Show a) => Vector a -> String
showVector = bracket2 "(" ")" . unwords . map show . toList

instance Num a => VectorSpace Vector a where
  (|+|) = vectorAddition
  (|-|) = vectorSubtraction
  (.*) = scalarMultiplication
  toScalar = toScalar
  fromScalar = fromScalar
  isScalar = isScalar

instance Floating r => MetricSpace r (Vector r) where
  distance v w = euclidianNorm (v `vectorSubtraction` w)

instance Floating r => NormedVectorSpace Vector r where
  norm = euclidianNorm

instance Floating r => InnerProductSpace Vector r where
  (|.|) = dotProduct

infixl 7 ><

(><) :: Num a => Vector a -> Vector a -> Vector a
(><) = crossProduct

instance Floating r => Num (Vector r) where
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
    | otherwise = fromScalar (x |.| y)

instance Floating r => Fractional (Vector r) where
  fromRational = fromScalar . fromRational
  recip x
    | isScalar x = fromScalar (recip (toScalar x))
    | otherwise = errorNotScalar

--------------------------------------------------------------------------------
-- Scalars
--------------------------------------------------------------------------------

fromScalar :: a -> Vector a
fromScalar x = Vector $ Array.listArray (1,1) [x]

isScalar :: Vector a -> Bool
isScalar v = dim v == 1

toScalar :: Vector a -> a
toScalar v
  | isScalar v = v ! 1
  | otherwise = errorNotScalar

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

vector :: (Int -> a) -> Int -> Vector a
vector f n = Vector $ Array.listArray (1,n) [ f i | i <- [1 .. n] ]

zero :: Num a => Int -> Vector a
zero = vector (\_ -> 0)

ones :: Num a => Int -> Vector a
ones = vector (\_ -> 1)

unit :: Num a => Int -> Int -> Vector a
unit n j = vector (\i -> if i == j then 1 else 0) n

fromList :: [a] -> Vector a
fromList xs = Vector $ Array.listArray (1, length xs) xs

linearCombination :: Num a => [(a, Vector a)] -> Vector a
linearCombination = List.foldl1' (|+|) . (map (uncurry (.*)))

standardBasis :: Num a => Int -> [Vector a]
standardBasis n = map (unit n) [1 .. n]

--------------------------------------------------------------------------------
-- Deconstrution
--------------------------------------------------------------------------------

(!) :: Vector a -> Int -> a
v ! n = component n v

component :: Int -> Vector a -> a
component n v = toArray v Array.! n

toList :: Vector a -> [a]
toList v = [ v ! i | i <- [1 .. dim v] ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

dim :: Vector a -> Int
dim = snd . Array.bounds . toArray

isUnit :: (Eq a, Floating a) => Vector a -> Bool
isUnit v = euclidianNorm v == 1.0

pivot :: (Eq a, Num a) => Vector a -> Int
pivot v
  = List.foldl'
      (\r i -> if v ! i /= 0 then min i r else r)
      (dim v + 1)
      ([1 .. dim v] :: [Int])

--------------------------------------------------------------------------------
-- Arithmetic
--------------------------------------------------------------------------------

componentWise :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
componentWise f a b
  | dim a /= dim b
    = errorDimNotEq
  | otherwise
    = Vector $ Array.listArray (1, n) [ f (a ! i) (b ! i) | i <- [1 .. n] ]
  where n = dim a

vectorAddition :: Num a => Vector a -> Vector a -> Vector a
vectorAddition = componentWise (+)

vectorSubtraction :: Num a => Vector a -> Vector a -> Vector a
vectorSubtraction = componentWise (-)

scalarMultiplication :: Num a => a -> Vector a -> Vector a
scalarMultiplication k = fmap (k*)

dotProduct :: Num a => Vector a -> Vector a -> a
dotProduct a b
  | dim a /= dim b
    = errorDimNotEq
  | otherwise
    = sum $ toList $ componentWise (*) a b

euclidianNorm :: Floating a => Vector a -> a
euclidianNorm x = sqrt (dotProduct x x)

crossProduct :: Num a => Vector a -> Vector a -> Vector a
crossProduct v w
  | (dim v, dim w) == (3, 3)
    = fromList
      [ v!2 * w!3 - v!3 * w!2
      , v!3 * w!1 - v!1 * w!3
      , v!1 * w!2 - v!2 * w!1
      ]
  | (dim v, dim w) == (7, 7)
    = fromList
      [  (v!2 * w!3) - v!3 * w!2 + v!4 * w!5 - v!5 * w!4 - v!6 * w!7 + v!7 * w!6
      , -(v!1 * w!3) + v!3 * w!1 + v!4 * w!6 + v!5 * w!7 - v!6 * w!4 - v!7 * w!5
      ,  (v!1 * w!2) - v!2 * w!1 + v!4 * w!7 - v!5 * w!6 + v!5 * w!6 - v!7 * w!4
      , -(v!1 * w!5) - v!2 * w!6 - v!3 * w!7 + v!5 * w!1 + v!6 * w!2 + v!7 * w!3
      ,  (v!1 * w!4) - v!2 * w!7 + v!3 * w!6 - v!4 * w!1 - v!6 * w!3 + v!7 * w!2
      ,  (v!1 * w!7) + v!2 * w!4 - v!3 * w!5 - v!4 * w!2 + v!5 * w!3 - v!7 * w!1
      , -(v!1 * w!6) + v!2 * w!5 + v!3 * w!4 - v!4 * w!3 - v!5 * w!2 + v!6 * w!1
      ]
  | otherwise
    = errorDimNot3Or7

tripleProduct :: Num a => Vector a -> Vector a -> Vector a -> a
tripleProduct a b c = (a `crossProduct` b) `dotProduct` c

--------------------------------------------------------------------------------
-- Comparison
--------------------------------------------------------------------------------

(|||) :: (Eq a, Fractional a) => Vector a -> Vector a -> Bool
(|||) = parallel

-- FIXME: not Fractional
parallel :: (Eq a, Fractional a) => Vector a -> Vector a -> Bool
parallel v w = equal (toList (componentWise (/) v w))

(-|-) :: (Eq a, Num a) => Vector a -> Vector a -> Bool
(-|-) = orthogonal

orthogonal :: (Eq a, Num a) => Vector a -> Vector a -> Bool
orthogonal v w = dotProduct v w == 0

-- orthonormal
--  = all unit && pairwise orthongonal

--------------------------------------------------------------------------------
-- Projection
--------------------------------------------------------------------------------

orthogonalProjection :: Fractional a => Vector a -> Vector a -> Vector a
orthogonalProjection v a
  = (dotProduct a v / dotProduct a a) `scalarMultiplication` a

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

errorNotScalar
  = error "errorNotScalar"

errorDimNotEq
  = error "errorDimNotEq"

errorDimNot3Or7
  = error "errorDimNot3Or7"
