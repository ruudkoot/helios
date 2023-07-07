module Helios.Data.List
( module Data.List
, module Data.List.Extra
, IsList(..)
, isSingleton
, slide
, unzipL
, zipListWith
, mapWithPreceding
, padTo
, pad
, dropAt
, insertAt
, updateAt
, updateAt'
, updatesAt
, cutAt
, pasteAt
) where

import Data.List
import Data.List.Extra
import GHC.Exts ( IsList(..) )

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

slide :: (a -> a -> b) -> [a] -> [b]
slide f (x1:x2:[]) = [f x1 x2]
slide f (x1:x2:xs) = f x1 x2 : slide f (x2:xs)

unzipL :: [(a,a)] -> [[a]]
unzipL xs = let (x,y) = unzip xs in [x,y]

zipListWith :: ([a] -> b) -> [[a]] -> [b]
zipListWith f = map f . transpose

mapWithPreceding
  :: forall a b. ([b] -> a -> b) -> [a] -> [b]
mapWithPreceding f
  = mapWithPreceding' []
  where
    mapWithPreceding'
      :: [b] -> [a] -> [b]
    mapWithPreceding' ys []
      = []
    mapWithPreceding' ys (x:xs)
      = let y = f ys x in y : mapWithPreceding' (ys ++ [y]) xs

--------------------------------------------------------------------------------
-- Padding
--------------------------------------------------------------------------------

padTo :: Int -> a -> [a] -> [a]
padTo n x xs
  = xs ++ replicate (n - length xs) x

pad :: a -> [[a]] -> [[a]]
pad x xss
  = map (padTo (maximum (map length xss)) x) xss

--------------------------------------------------------------------------------
-- 'at'
--------------------------------------------------------------------------------

dropAt :: Int -> Int -> [a] -> [a]
dropAt n m xs
  = xs1 ++ drop m xs2
  where
    (xs1, xs2) = splitAt n xs

insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs
  = xs1 ++ x : xs2
  where
    (xs1, xs2) = splitAt n xs

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt n f xs
  = xs1 ++ f x : xs2
  where
    (xs1, x:xs2) = splitAt n xs

updateAt' :: Int -> (a -> (b, a)) -> [a] -> (b, [a])
updateAt' n f xs
  = (y', xs1 ++ y : xs2)
  where
    (xs1, x:xs2) = splitAt n xs
    (y', y) = f x

updatesAt :: Int -> Int -> ([a] -> [a]) -> [a] -> [a]
updatesAt n m f xs0
  = xs1 ++ f xs3 ++ xs4
  where
    (xs1, xs2) = splitAt n xs0
    (xs3, xs4) = splitAt m xs2

cutAt :: Int -> Int -> [a] -> ([a], [a])
cutAt pos len xs0
  = (xs3, xs1 ++ xs4)
  where
    (xs1, xs2) = splitAt pos xs0
    (xs3, xs4) = splitAt len xs2

pasteAt :: Int -> [a] -> [a] -> [a]
pasteAt pos ys xs
  = xs1 ++ ys ++ xs2
  where
    (xs1, xs2) = splitAt pos xs