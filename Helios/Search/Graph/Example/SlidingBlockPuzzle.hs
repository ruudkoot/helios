{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Helios.Search.Graph.Example.SlidingBlockPuzzle
( Tile(..)
, Board(..)
, board
, Action(..)
, act
, problem
) where

import qualified Helios.Data.Array as Array
import qualified Helios.Data.List as List
import qualified Helios.Data.Maybe as Maybe

import           Helios.Search.Graph

data Tile
  = Blank
  | Tile Int
  deriving (Eq, Show)

data Board
  = Board
    { width :: Int
    , tiles :: Array.Array (Int, Int) Tile
    , blank :: (Int, Int)
    }
  deriving (Eq, Show)

board :: Int -> Board
board n
  = Board
    { width
        = n
    , tiles
        = Array.array ((0,0),(n-1,n-1))
        $ [((r,c), mkTile r c) | r <- [0..n-1], c <- [0..n-1]]
    , blank
        = (n-1, n-1)
    }
  where
    mkTile r c
      | r == n-1, c == n-1
        = Blank
      | otherwise
        = Tile (n*r+c+1)

fromList :: [[Tile]] -> Board
fromList tss
  = Board
    { width
        = n
    , tiles
        = Array.array ((0,0),(n-1,n-1))
        $ [((r,c), tss !! r !! c) | r <- [0..n-1], c <- [0..n-1]]
    , blank
        = Maybe.fromJust (List.elemIndex Blank (concat tss)) `divMod` n
    }
  where
    n = length tss

data Action = LEFT | RIGHT | UP | DOWN
  deriving (Eq, Show)

act :: Board -> Action -> Maybe Board
act board action
  | blank' <- loc (blank board) action, valid blank'
    = Just (swapBlank blank' board)
  | otherwise
    = Nothing
  where
    valid (r,c)     = 0 <= r && r < width board && 0 <= c && c < width board

    loc (r,c) LEFT  = (r,c-1)
    loc (r,c) RIGHT = (r,c+1)
    loc (r,c) UP    = (r-1,c)
    loc (r,c) DOWN  = (r+1,c)

swapBlank :: (Int, Int) -> Board -> Board
swapBlank pos Board{..}
  = Board
    { width = width
    , tiles = tiles Array.// [(pos, Blank), (blank, tiles Array.! pos)]
    , blank = pos
    }

problem :: Board -> Problem Action Board
problem initialBoard
  = Problem
    { initialState
        = initialBoard
    , successorFn
        = \ b ->
            Maybe.mapMaybe (\action -> (action,) <$> act b action)
                [LEFT, RIGHT, UP, DOWN]
    , goalTest
        = \ b -> b == board (width b)
    , stepCost
        = \ _ _ _ -> 1
    }

example1 :: Problem Action Board
example1
  = problem $ fromList
    [[Tile 1, Tile 2, Tile 3]
    ,[Tile 4, Tile 8, Tile 5]
    ,[Tile 7, Blank , Tile 6]
    ]

-- is this solvable? goal state has blank at top-left
example2 :: Problem Action Board
example2
  = problem $ fromList
    [[Tile 7, Tile 2, Tile 4]
    ,[Tile 5, Blank,  Tile 6]
    ,[Tile 8, Tile 3, Tile 1]
    ]
