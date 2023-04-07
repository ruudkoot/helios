module Helios.Data.BinomialTree
( BinomialTree
, head
, last
, take
, map
, zip
, unfold
, fold
, forward
, backward
) where

import           Prelude hiding (head, last, map, take, zip)
import qualified Helios.Data.List as List

newtype BinomialTree a
  = BinomialTree [[a]]
  deriving (Eq, Show)

head :: BinomialTree a -> a
head (BinomialTree ls)
  = List.head (List.head ls)

last :: BinomialTree a -> [a]
last (BinomialTree ls)
  = List.last ls

take :: Int -> BinomialTree a -> BinomialTree a
take n (BinomialTree ls)
  = BinomialTree (List.take n ls)

map :: (a -> b) -> BinomialTree a -> BinomialTree b
map f (BinomialTree ls)
  = BinomialTree (List.map (List.map f) ls)

zip :: BinomialTree a -> BinomialTree b -> BinomialTree (a, b)
zip (BinomialTree xs) (BinomialTree ys)
  = BinomialTree (List.zipWith List.zip xs ys)

-- | 'f' and 'g' must commute.
unfold :: (a -> a) -> (a -> a) -> a -> BinomialTree a
unfold f g x
  = BinomialTree $ iterate (\ys@(y:_) -> f y : List.map g ys) [x]

fold :: (a -> a -> a) -> [a] -> BinomialTree a
fold f xs
  = BinomialTree (reverse (fold' xs))
  where
    fold' [x] = [[x]]
    fold' xs  = xs : fold' (List.slide f xs)

slide3 :: (a -> b -> b -> c) -> [a] -> [b] -> [c]
slide3 f [x]    (y1:y2:[]) = [f x y1 y2]
slide3 f (x:xs) (y1:y2:ys) = f x y1 y2 : slide3 f xs (y2:ys)

forward :: (a -> a -> a -> b) -> BinomialTree a -> BinomialTree b
forward f (BinomialTree ls)
  = BinomialTree (forward' ls)
  where
    forward' [_]        = []
    forward' (x1:x2:xs) = slide3 f x1 x2 : forward' (x2:xs)

backward :: (a -> a -> a -> a) -> BinomialTree a -> BinomialTree a
backward f (BinomialTree ls)
  = BinomialTree (backward' ls)
  where
    backward' [x]    = [x]
    backward' (x:xs) = let (y:ys) = backward' xs in slide3 f x y : y : ys
