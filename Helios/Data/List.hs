module Helios.Data.List
( module Data.List
, module Data.List.Extra
, IsList(..)
, isSingleton
, slide
, unzipL
, padTo
, pad
, dropAt
, insertAt
, updateAt
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