-- finite / infinite relations
-- relations as graphs (adjacency list, adjacency matrix)
-- relations as functions
-- relations from combinators

module Helios.Logic.Relation where

import Helios.Logic.Infinite

--------------------------------------------------------------------------------
-- Finite types
--------------------------------------------------------------------------------

newtype CHAR = CHAR { unCHAR :: Char }
  deriving ( Eq, Ord )

instance Show CHAR where
  show (CHAR c) = [c]

instance Enumerable CHAR where
  enumerate = CHAR <$> ['A'..'Z']

--------------------------------------------------------------------------------
-- Relations
--------------------------------------------------------------------------------

newtype Relation a b = Relation { unRelation :: [(a,b)] }
  deriving ( Show )

empty :: Relation a b
empty = Relation []

inverse :: Relation a b -> Relation b a
inverse (Relation r) =
  Relation
  [ (y, x)
  | (x, y) <- r
  ]

compose :: (Eq b) => Relation a b -> Relation b c -> Relation a c
compose (Relation r) (Relation s) =
  Relation
  [ (a, c)
  | (a, b1) <- r
  , (b2, c) <- s
  , b1 == b2
  ]

union :: Relation a b -> Relation a b -> Relation a b
union (Relation r) (Relation s) = Relation (interleave r s)

unions :: (i -> Relation a b) -> [i] -> Relation a b
unions _ [] =
  empty
unions k (x:xs) =
  Relation (interleave (unRelation (k x)) (unRelation (unions k xs)))

--------------------------------------------------------------------------------
-- Reductions
--------------------------------------------------------------------------------

type Reduction a = Relation a a

identity :: (Enumerable a) => Reduction a
identity =
  Relation
  [ (x, x)
  | x <- enumerate
  ]

-- duplicates
composeN :: (Enumerable a, Eq a) => Int -> Reduction a -> Reduction a
composeN 0 _ = identity
composeN n r = composeN (n - 1) r `compose` r

-- non-terminating
transitiveClosure
  :: (Enumerable a, Eq a) => Reduction a -> Reduction a
transitiveClosure r
  = unions (\i -> composeN i r) [1..]

-- non-terminating
reflexiveTransitiveClosure
  :: (Enumerable a, Eq a) => Reduction a -> Reduction a
reflexiveTransitiveClosure r
  = union identity (transitiveClosure r)

reflexiveClosure
  :: (Enumerable a) => Reduction a -> Reduction a
reflexiveClosure r =
  union identity r

symmetricClosure
  :: Reduction a -> Reduction a
symmetricClosure r =
  union r (inverse r)

transitiveSymmetricClosure
  :: (Enumerable a, Eq a) => Reduction a -> Reduction a
transitiveSymmetricClosure =
  transitiveClosure . symmetricClosure

reflexiveTransitiveSymmetricClosure
  :: (Enumerable a, Eq a) => Reduction a -> Reduction a
reflexiveTransitiveSymmetricClosure =
  reflexiveTransitiveClosure . symmetricClosure

-- P closure of R (need closed under arbitrary intersections)

reducable :: (Eq a) => Reduction a -> a -> Bool
reducable (Relation r) x =
  (not . null)
  [ x'
  | (x', _) <- r
  , x == x'
  ]

-- (normal form)
irreducable :: (Eq a) => Reduction a -> a -> Bool
irreducable r = not . reducable r

-- (direct) successor

joinable :: (Enumerable a, Ord a) => Reduction a -> a -> a -> Bool
joinable r x y = (not . null) (intersect x' y')
  where r' = reflexiveTransitiveClosure r
        x' = [ z | (z, _) <- unRelation r' ]
        y' = [ z | (z, _) <- unRelation r' ]

-- Church-Rosser    x <-*-> y => x \|/ y
-- confluent        y1 <-*- x -*-> y2 => y1 \|/ y2
-- terminating      no infinite chain
-- normalizing      every element has normal form
-- convergent       confluent & terminating

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

newtype Natural2 = Natural2 { unNatural2 :: Integer }
  deriving ( Eq, Ord )

instance Show Natural2 where
  show (Natural2 n) = show n

instance Enumerable Natural2 where
  enumerate = Natural2 <$> [2..]

divides :: Integral a => a -> a -> Bool
divides n m = m `mod` n == 0

divisors :: Reduction Natural2
divisors =
  Relation
  [ (m, n)
  | (m, n) <- enumerate
  , m > n
  , unNatural2 n `divides` unNatural2 m
  ]

newtype Words2 = Words2 { unWords2 :: String }
  deriving ( Eq, Ord )

instance Show Words2 where
  show (Words2 []) = "ε"
  show (Words2 u) = u

instance Enumerable Words2 where
  enumerate = Words2 <$> wordsOver "ab"

