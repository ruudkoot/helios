{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Helios.Data.DisjointSets
  ( Representative
  , DisjointSets
  , toLists
  , makeSets
  , union
  , findSet
  ) where

import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Maybe

newtype Representative = R { unR :: Int }
  deriving ( Enum, Eq, Num, Show )

data DisjointSets a
  = DisjointSets
    { representatives :: Map.Map a Representative
    , sets :: IntMap.IntMap [a]
    }
  deriving ( Show )

toLists :: DisjointSets a -> [[a]]
toLists = IntMap.elems . sets

makeSets :: (Ord a) => [a] -> DisjointSets a
makeSets xs =
  DisjointSets
    (Map.fromList (zip xs [0..]))
    (IntMap.fromList (zipWith (\n x -> (n,[x])) [0..] xs))

union :: (Ord a) => DisjointSets a -> a -> a -> DisjointSets a
union disjointSets x y =
  if rx == ry
  then
    disjointSets
  else
    DisjointSets
      (foldr (flip Map.insert rx) (representatives disjointSets) ys)
      (IntMap.delete (unR ry)
        (IntMap.insert (unR rx) (xs ++ ys) (sets disjointSets))
      )
  where rx = findSet disjointSets x
        ry = findSet disjointSets y
        xs = sets disjointSets IntMap.! (unR rx)
        ys = sets disjointSets IntMap.! (unR ry)

findSet :: (Ord a) => DisjointSets a -> a -> Representative
findSet disjointSets x =
  fromMaybe (error "findSet")
    (Map.lookup x (representatives disjointSets))
