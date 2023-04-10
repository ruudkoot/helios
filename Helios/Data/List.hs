module Helios.Data.List
( module Data.List
, module Data.List.Extra
, IsList(..)
, isSingleton
, slide
, unzipL
) where

import Data.List
import Data.List.Extra
import GHC.Exts ( IsList(..) )

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

slide :: (a -> a -> a) -> [a] -> [a]
slide f (x1:x2:[]) = [f x1 x2]
slide f (x1:x2:xs) = f x1 x2 : slide f (x2:xs)

unzipL :: [(a,a)] -> [[a]]
unzipL xs = let (x,y) = unzip xs in [x,y]
