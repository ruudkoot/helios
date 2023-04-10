{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Helios.Logic.Infinite
  ( Enumerable(..)
  , interleave
  , interleaves
  , wordsOver
  , diagonal
  , intersect
  , finiteCross
  , infiniteCross
  ) where

import           Data.List ( nub )
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Enumerable types
--------------------------------------------------------------------------------

class Enumerable a where
  enumerate :: [a]

instance Enumerable Bool where
  enumerate = enumFrom False

instance Enumerable Int where
  enumerate = 0 : interleave (enumFrom 1) (enumFromThen (-1) (-2))

instance Enumerable Integer where
  enumerate = 0 : interleave (enumFrom 1) (enumFromThen (-1) (-2))

instance (Enumerable a, Enumerable b) => Enumerable (a,b) where
  enumerate = diagonal (map (\x -> map (x,) enumerate) enumerate)

-- FIXME: not balanced
instance (Enumerable a, Enumerable b, Enumerable c) => Enumerable (a,b,c) where
  enumerate = map (\((x,y),z) -> (x,y,z)) enumerate


--------------------------------------------------------------------------------
-- Infinite lists
--------------------------------------------------------------------------------

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

interleaves :: [[a]] -> [a]
interleaves = interleaves' []
  where interleaves' []  []           = []
        interleaves' yss []           = interleaves' [] (reverse yss)
        interleaves' yss ([]:xss)     = interleaves' yss xss
        interleaves' yss ((x:xs):xss) = x : interleaves' (xs:yss) xss

wordsOver :: [a] -> [[a]]
wordsOver cs = [] : interleaves (map (\c -> map (c:) (wordsOver cs)) cs)

diagonal :: [[a]] -> [a]
diagonal = concat . foldr diags []
  where diags []     ys = ys
        diags (x:xs) ys = [x] : merge xs ys

        merge []       ys     = ys
        merge xs@(_:_) []     = map (:[]) xs
        merge (x:xs)   (y:ys) = (x:y) : merge xs ys

intersect :: forall a. (Ord a) => [a] -> [a] -> [a]
intersect = intersect' Set.empty Set.empty
  where intersect' :: Set.Set a -> Set.Set a -> [a] -> [a] -> [a]
        intersect' _  _  [] [] = []
        intersect' sx sy [] ys = intersect' sy sx ys []
        intersect' sx sy (x:xs) ys
          | x `Set.member` sx = intersect' sy sx ys xs
          | x `Set.member` sy = x : intersect' sy (Set.insert x sx) ys xs
          | otherwise         = intersect' sy (Set.insert x sx) ys xs

finiteCross :: [[a]] -> [[a]]
finiteCross []       = [[]] -- FIXME: ??
finiteCross (xs:xss) = [ x : xx | x <- xs, xx <- finiteCross xss ]

-- FIXME: finite list of infinite lists only
-- FIXME: slow due to nub and duplicates
-- FIXME: infinitePower :: [a] -> Int -> [[a]] more efficient?
infiniteCross :: (Eq a) => [[a]] -> [[a]]
infiniteCross = concat . cross' []
  where
    cross' :: (Eq a) => [[a]] -> [[a]] -> [[[a]]]
    cross' acc rest = newstuff : cross' (acc ++ newstuff) rest'
      where (h,rest') = chop rest
            hs = mkH h
            newstuff = nub (concatMap (mkNewStuff acc) hs) ++ [h]

    chop :: [[a]] -> ([a],[[a]])
    chop xs = (map head xs, map tail xs)

    mkH :: [a] -> [[Maybe a]]
    mkH = tail . init . mkH'
      where mkH' [] = [[]]
            mkH' (x:xs) = map (Nothing :) xs' ++ map (Just x :) xs'
              where xs' = mkH' xs

    -- FIXME: duplicates .. hence the nub in cross' above
    mkNewStuff :: (Eq a) => [[a]] -> [Maybe a] -> [[a]]
    mkNewStuff acc h = map (zipWith f h) acc
      where f Nothing y = y
            f (Just x) _ = x

