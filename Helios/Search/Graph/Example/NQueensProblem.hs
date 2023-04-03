{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Helios.Search.Graph.Example.NQueensProblem
( problem
, main
) where

import qualified Helios.Data.Array as Array
import qualified Helios.Data.List as List
import qualified Helios.Data.Maybe as Maybe
import           Helios.Data.Queue

import           Helios.Search.Graph

data Board
  = Board
    { size :: Int
    , queens :: [Int]
    }
  deriving (Show)

isValid :: Board -> Bool
isValid Board{..}
  = isValid' queens
  where
    isValid' []
      = True
    isValid' (q:qs)
      = isValid'' 1 qs && isValid' qs
      where
        isValid'' _ []
          = True
        isValid'' d (r:rs)
          = r /= q && abs (r - q) /= d && isValid'' (d+1) rs

data Action
  = Place Int
  deriving (Show)

act :: Board -> Action -> Board
act board (Place n)
  = board { queens = queens board ++ [n] }

problem :: Int -> Problem Action Board
problem n
  = Problem
    { initialState
        = Board { size = n, queens = [] }
    , successorFn
        = \ b ->
            filter (isValid . snd) $
              map (\action -> (action, act b action)) (map Place [1..n])
    , goalTest
        = \Board{..} -> length queens == size
    , stepCost
        = \ _ _ _ -> 0 -- FIXME: Nothing
    }

solve :: Int -> [[Int]]
solve n
  = map (queens . state . head . unPath)
  $ depthFirstSearch (problem n)

main :: IO ()
main = do
  print $ solve 15
