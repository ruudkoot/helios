{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Helios.Data.Matrix
( Matrix(toArray)
, columnVector
, rowVector
, matrix
, zero
, ones
, eye
, rows
, cols
, fromRowLists
, isVector
, isSquare
, foldMatrix
, isDiagonal
, isSymmetric
, matrixAddition
, matrixDifference
, scalarMultiplication
, transpose
, matrixMultiplication, (|*|)
, matrixExponentiation
, determinant
, mapRows
, pivots
, gaussJordan
) where

import qualified Helios.Class.Space as Space
import           Helios.Class.Space
  hiding ( fromScalar, toScalar, isScalar )

import qualified Helios.Data.Array as Array
import qualified Helios.Data.List as List
import qualified Helios.Data.Vector as Vector

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- FIXME: unboxed array
-- FIXME: lazy multiplication
newtype Matrix a = Matrix { toArray :: Array.Array (Int, Int) a }
  deriving (Eq, Ord, Functor)

instance Show a => Show (Matrix a) where
  show = showMatrix

showMatrix :: (Show a) => Matrix a -> String
showMatrix = List.intercalate "\n" . map (unwords . map show) . toRowLists

instance Num a => VectorSpace Matrix a where
  (|+|) = matrixAddition
  (|-|) = matrixDifference
  (.*) = scalarMultiplication
  toScalar = toScalar
  fromScalar = fromScalar
  isScalar = isScalar

infixl 7 |*|

(|*|) :: Num a => Matrix a -> Matrix a -> Matrix a
(|*|) = matrixMultiplication

instance Num a => Num (Matrix a) where
  fromInteger = fromScalar . fromInteger
  (+) = (|+|)
  (-) = (|-|)
  negate a = (-1) `scalarMultiplication` a
  abs = error "Matrix abs"
  signum = error "Matrix signum"
  x * y
    | isScalar x = toScalar x `scalarMultiplication` y
    | isScalar y = toScalar y `scalarMultiplication` x
    | otherwise = x |*| y

--------------------------------------------------------------------------------
-- Scalars
--------------------------------------------------------------------------------

fromScalar :: a -> Matrix a
fromScalar x = Matrix $ Array.listArray ((1,1),(1,1)) [x]

isScalar :: Matrix a -> Bool
isScalar a = dims a == (1,1)

toScalar :: Matrix a -> a
toScalar a
  | isScalar a = a ! (1,1)
  | otherwise = errorNotScalar

--------------------------------------------------------------------------------
-- Vectors
--------------------------------------------------------------------------------

columnVector :: Vector.Vector a -> Matrix a
columnVector v
  = matrix (\i _ -> v Vector.! i) n 1
  where n = Vector.dim v

rowVector :: Vector.Vector a -> Matrix a
rowVector v
  = matrix (\_ j -> v Vector.! j) 1 n
  where n = Vector.dim v

--------------------------------------------------------------------------------
-- Construction
--------------------------------------------------------------------------------

matrix :: (Int -> Int -> a) -> Int -> Int -> Matrix a
matrix f m n
  = Matrix $ Array.listArray ((1,1),(m,n)) [ f i j | i <- [1..m], j <- [1..n] ]

zero :: Num a => Int -> Int -> Matrix a
zero = matrix (\_ _ -> 0)

ones :: Num a => Int -> Int -> Matrix a
ones = matrix (\_ _ -> 1)

eye :: Num a => Int -> Matrix a
eye n = matrix (\i j -> if i == j then 1 else 0) n n

zeroC :: Num a => Int -> Matrix a
zeroC n = zero n 1

onesC :: Num a => Int -> Matrix a
onesC n = ones n 1

zeroR :: Num a => Int -> Matrix a
zeroR n = zero 1 n

onesR :: Num a => Int -> Matrix a
onesR n = ones 1 n

unitC :: Num a => Int -> Int -> Matrix a
unitC n j = matrix (\i _ -> if i == j then 1 else 0) n 1

unitR :: Num a => Int -> Int -> Matrix a
unitR n i = matrix (\_ j -> if i == j then 1 else 0) 1 n

-- FIXME: check all lists of equal length
fromRowLists :: [[a]] -> Matrix a
fromRowLists xss@(xs:_) = Matrix $ Array.listArray ((1,1),(r,c)) (concat xss)
  where r = length xss
        c = length xs

-- transpose

--------------------------------------------------------------------------------
-- Deconstrution
--------------------------------------------------------------------------------

(!) :: Matrix a -> (Int, Int) -> a
v ! (m,n) = component m n v

component :: Int -> Int -> Matrix a -> a
component m n a = toArray a Array.! (m,n)

row :: Matrix a -> Int -> Vector.Vector a
row m i
  = Vector.vector (\j -> m ! (i,j)) (cols m)

toRowVectors :: Matrix a -> [Vector.Vector a]
toRowVectors = undefined

column :: Matrix a -> Int -> Vector.Vector a
column m j
  = Vector.vector (\i -> m ! (i,j)) (rows m)

toColumnVectors :: Matrix a -> [Vector.Vector a]
toColumnVectors = undefined

toRowLists :: Matrix a -> [[a]]
toRowLists m
  = [ [ m ! (i,j) | j <- [1 .. cols m] ] | i <- [1 .. rows m] ]

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

dims :: Matrix a -> (Int, Int)
dims = snd . Array.bounds . toArray

rows :: Matrix a -> Int
rows = fst . snd . Array.bounds . toArray

cols :: Matrix a -> Int
cols = snd . snd . Array.bounds . toArray

size :: Matrix a -> Int
size m = rows m * cols m

isVector :: Matrix a -> Bool
isVector a = m == 1 || n == 1 where (m,n) = dims a

isSquare :: Matrix a -> Bool
isSquare m = rows m == cols m

foldMatrix :: (Int -> Int -> a -> p -> p) -> p -> Matrix a -> p
foldMatrix f e m
  = List.foldl' (\r (i,j) -> f i j (m ! (i,j)) r) e
                [ (i,j) | i <- [1 .. rows m], j <- [1 .. cols m] ]

caseUDL :: Int -> Int -> (a -> b) -> (a -> b) -> (a -> b) -> a -> b
caseUDL i j fu fd fl
  = case compare i j of
      LT -> fu
      EQ -> fd
      GT -> fl

testUDL :: (a -> Bool) -> (a -> Bool) -> (a -> Bool) -> Matrix a -> Bool
testUDL pu pd pl
  = foldMatrix (\i j x r -> caseUDL i j pu pd pl x && r) True

isDiagonal :: (Eq a, Num a) => Matrix a -> Bool
isDiagonal m = isSquare m && testUDL (==0) (const True) (==0) m

isSymmetric :: Eq a => Matrix a -> Bool
isSymmetric m
  = isSquare m && foldMatrix (\i j x r -> r && x == m ! (j,i)) True m

--------------------------------------------------------------------------------
-- Arithmetic
--------------------------------------------------------------------------------

componentWise :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
componentWise f a b
  | dims a /= dims b
    = errorDimsNotEq
  | otherwise
    = Matrix
    $ Array.listArray
        ((1,1),(m,n))
        [ f (a ! (i,j)) (b ! (i,j))
        | i <- [1 .. m]
        , j <- [1 .. n]
        ]
  where (m,n) = dims a

matrixAddition :: Num a => Matrix a -> Matrix a -> Matrix a
matrixAddition = componentWise (+)

matrixDifference :: Num a => Matrix a -> Matrix a -> Matrix a
matrixDifference = componentWise (-)

scalarMultiplication :: Num a => a -> Matrix a -> Matrix a
scalarMultiplication k = fmap (k*)

infixl 7 .*

(.*) = scalarMultiplication

transpose :: Matrix a -> Matrix a
transpose a
  = matrix (\i j -> a ! (j,i)) n m
  where (m,n) = dims a

-- FIXME: add special case for vector inner / dot product
-- FIXME: not efficient
matrixMultiplication :: Num a => Matrix a -> Matrix a -> Matrix a
matrixMultiplication a b
  | n == p
    = Matrix
    $ Array.listArray
        ((1,1),(m,q))
        [ Vector.dotProduct (row a i) (column b j)
        | i <- [1 .. m]
        , j <- [1 .. q]
        ]
  | otherwise
    = errorMatrixMultiplication m n p q
  where
    m = rows a
    n = cols a
    p = rows b
    q = cols b

-- FIXME: not efficient
matrixExponentiation :: Num a => Matrix a -> Int -> Matrix a
matrixExponentiation a 0
  | isSquare a
    = eye (rows a)
matrixExponentiation a 1
  | isSquare a
    = a
matrixExponentiation a n
  | isSquare a
    = matrixMultiplication a (matrixExponentiation a (n - 1))
matrixExponentiation a n
  = errorNotSquare

{- use generioc vector lifint
euclidianNorm :: Floating a => Matrix a -> a
euclidianNorm a
  | isVector a
    = sqrt . sum . map (^2) . elems . toArray $ a
  | otherwise
    = errorNotAVector

angle a b
  | isVector a, isVector b
    = (a * b)
  | otherwise
    = errorNotAVector
-}

--------------------------------------------------------------------------------
-- Determinants
--------------------------------------------------------------------------------

alternates :: Num a => [a] -> [a]
alternates [] = []
alternates [x] = [x]
alternates (x0:x1:xs) = x0 : -x1 : alternates xs

pick :: [a] -> [(a,[a])]
pick xs
  = map (\n -> (xs !! n, take n xs ++ drop (n + 1) xs)) [0 .. length xs - 1]

determinant :: Num a => Matrix a -> a
determinant a
  | isSquare a
    = determinant' 1 [1 .. cols a] a
  | otherwise
    = errorNotSquare

determinant' :: Num a => Int -> [Int] -> Matrix a -> a
determinant' _ [] a
  = 1
determinant' row cols a
  = sum (alternates (map f (pick cols)))
  where f (c,cs) = a ! (row,c) * determinant' (row + 1) cs a

--------------------------------------------------------------------------------
-- Gauss-Jordan elemination
--------------------------------------------------------------------------------

-- TODO: Cramer's rule

mapRows :: (Vector.Vector a -> b) -> Matrix a -> Vector.Vector b
mapRows f a = Vector.vector (f . row a) (rows a)

pivots :: (Eq a, Num a) => Matrix a -> Vector.Vector Int
pivots = mapRows Vector.pivot

gaussJordan :: Matrix a -> Matrix b -> (Matrix a, Matrix b)
gaussJordan a b = uncurry jordan (gauss a b)

gauss :: Matrix a -> Matrix b -> (Matrix a, Matrix b)
gauss = undefined

jordan :: Matrix a -> Matrix b -> (Matrix a, Matrix b)
jordan = undefined

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

errorNotScalar
  = error "errorNotScalar"

errorMatrixMultiplication
  = error "errorMatrixMultiplication"

errorDimsNotEq
  = error "errorDimsNotEq"

errorNotAVector
  = error "errorNotAVector"

errorNotSquare
  = error "errorNotSquare"
