{-# LANGUAGE RecordWildCards #-}
module Helios.Optimize.VehicleRouting
( module Helios.Optimize.VehicleRouting.Types
, mkProblem, mkProblemRandomStart
, evaluate, evaluateRoute
, localSearch
) where

import           Helios.Control.Monad
import qualified Helios.Control.Random as Random
import           Helios.Data.List

import           Helios.Optimize.VehicleRouting.Types

--------------------------------------------------------------------------------
-- Problem
--------------------------------------------------------------------------------

mkProblem :: (Loc, [Loc]) -> Problem
mkProblem (depot, locs)
  = Problem
    { depot = depot
    , routes = locs : replicate 4 []
    }

mkProblemRandomStart :: (Loc, [Loc]) -> IO Problem
mkProblemRandomStart (depot, locs) = do
  routes' <- Random.partitionU 4 locs
  return $
    Problem
    { depot = depot
    , routes = [] : routes'
    }

--------------------------------------------------------------------------------
-- Evaluate
--------------------------------------------------------------------------------

manhattanDistance :: Loc -> Loc -> Int
manhattanDistance (Loc a b) (Loc c d)
  = abs (a-c) + abs (b-d)

evaluateRoute :: Loc -> [Loc] -> Int
evaluateRoute depot route
  = sum (slide manhattanDistance (depot : depot : route))

evaluate :: Problem -> (Int, Int, Int)
evaluate problem
  = ( length (routes problem !! 0)
    , maximum (map (evaluateRoute (depot problem)) (tail (routes problem)))
    , sum     (map (evaluateRoute (depot problem)) (tail (routes problem)))
    )

best :: [Problem] -> ((Int, Int, Int), Problem)
best ps
  = let p = minimumOn evaluate ps in (evaluate p, p)

--------------------------------------------------------------------------------
-- Mutate
--------------------------------------------------------------------------------

mutateMovePoint :: Int -> Int -> Int -> Int -> Problem -> Problem
mutateMovePoint routeS posS routeT posT problem
  = problem
    { routes
        = insert routeT posT
        $ remove routeS posS
        $ routes problem
    }
  where
    remove :: Int -> Int -> [[Loc]] -> (Loc, [[Loc]])
    remove route pos routes
      = (routes !! route !! pos, updateAt route (dropAt pos 1) routes)
    insert :: Int -> Int -> (Loc, [[Loc]]) -> [[Loc]]
    insert route pos (loc, routes)
      = updateAt route (insertAt pos loc) routes

allMovePoint :: Problem -> [Problem]
allMovePoint problem
  = [ mutateMovePoint routeS posS routeT posT problem
    | routeS <- [1 .. 4]
    , posS <- [0 .. length (routes problem !! routeS) - 1]
    , routeT <- [1 .. 4]
    , -- FIXME: Generates duplicates if routeS == routeT
      posT <- [0 .. length (routes problem !! routeT)]
    ]

mutateSwapPoint :: Int -> Int -> Int -> Int -> Problem -> Problem
mutateSwapPoint routeA posA routeB posB problem
  = problem
    { routes
        = update routeB posB locA
        $ update routeA posA locB
        $ routes problem
    }
  where
    locA = routes problem !! routeA !! posA
    locB = routes problem !! routeB !! posB

    update :: Int -> Int -> Loc -> [[Loc]] -> [[Loc]]
    update route pos loc
      = updateAt route (updateAt pos (const loc))

allSwapPoint :: Problem -> [Problem]
allSwapPoint problem
  = [ mutateSwapPoint routeS posS routeT posT problem
    | routeS <- [1 .. 4]
    , posS <- [0 .. length (routes problem !! routeS) - 1]
    , routeT <- [1 .. 4]
    , posT <- [0 .. length (routes problem !! routeT) - 1]
    , not (routeS == routeT && posS == posT)
    ]

mutate2optIntra :: Int -> Int -> Int -> Problem -> Problem
mutate2optIntra route pos len problem
  = problem
    { routes = updateAt route (updatesAt pos len reverse) (routes problem)
    }

all2optIntra :: Problem -> [Problem]
all2optIntra problem
  = [ mutate2optIntra route pos len problem
    | route <- [1 .. 4]
    , len <- [2 .. length (routes problem !! route)]
    , pos <- [0 .. length (routes problem !! route) - len]
    ]

mutateMoveSegment
  :: Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Bool
  -> Problem
  -> Problem
mutateMoveSegment routeS posS lenS routeT posT reverseT problem
  = problem { routes = insert (remove (routes problem)) }
  where
    remove :: [[Loc]] -> ([Loc], [[Loc]])
    remove routes
      = updateAt' routeS (cutAt posS lenS) routes
    insert :: ([Loc], [[Loc]]) -> [[Loc]]
    insert (seg, routes)
      | not reverseT
        = updateAt routeT (pasteAt posT seg) routes
      | otherwise
        = updateAt routeT (pasteAt posT (reverse seg)) routes

allMoveSegment :: Problem -> [Problem]
allMoveSegment problem
  = [ mutateMoveSegment routeS posS lenS routeT posT revT problem
    | routeS <- [1 .. 4]
    , posS <- [0 .. length (routes problem !! routeS) - 1]
    , lenS <- [1 .. 12] -- length (routes problem !! routeS)] -- FIXME
    , routeT <- [1 .. 4]
    , posT <- [0 .. length (routes problem !! routeT)]
    , revT <- [False, True]
    ]

-- FIXME: intra-route swaps
mutateSwapSegment
  :: Int
  -> Int
  -> Int
  -> Bool
  -> Int
  -> Int
  -> Int
  -> Bool
  -> Problem
  -> Problem
mutateSwapSegment routeA posA lenA revA routeB posB lenB revB problem
  = problem { routes = routes4 }
  where
    routes0 = routes problem
    (segA, routes1) = updateAt' routeA (cutAt posA lenA) routes0
    (segB, routes2) = updateAt' routeB (cutAt posB lenB) routes1
    segA' = if revB then reverse segA else segA
    segB' = if revA then reverse segB else segB
    routes3 = updateAt routeA (pasteAt posA segB') routes2
    routes4 = updateAt routeB (pasteAt posB segA') routes3

-- FIXME: generating duplicates (only for intra-route swaps?)
allSwapSegment :: Problem -> [(Problem,(Int,Int,Int,Bool,Int,Int,Int,Bool))]
allSwapSegment problem
  = [ ( mutateSwapSegment routeA posA lenA revA routeB posB lenB revB problem
      , (routeA, posA, lenA, revA, routeB, posB, lenB, revB)
      )
    | routeA <- [1 .. 4]
    , lenA <- [0 .. length (routes problem !! routeA)]
    , posA <- [0 .. length (routes problem !! routeA) - lenA]
    , routeB <- [routeA .. 4]
    , lenB <- [0 .. length (routes problem !! routeB)]
    , posB <- [(if routeA == routeB then posA + 1 else 0) .. length (routes problem !! routeB) - lenB]
    , revA <- if lenB > 1 then [False, True] else [False]
    , revB <- if lenA > 1 then [False, True] else [False]
    ]

allMutations :: Problem -> [Problem]
allMutations problem
  -- If we did not start from a random plan, then move all points from the
  -- unplanned state into the plan until everything has been planned.
  | notNull (routes problem !! 0)
    = [ mutateMovePoint 0 posS routeT 0 problem
      | posS <- [0 .. length (routes problem !! 0) - 1]
      , routeT <- [1 .. 4]
      ]
  | otherwise
    = concat
      [ []
  --  , allMovePoint problem    -- subsumed by swapSegment [test again]
  --  , allSwapPoint problem    -- subsumed by swapSegment [test again]
  --  , all2optIntra problem    -- subsumed by swapSegment [test again]
  --  , allMoveSegment problem  -- subsumed by swapSegment [test again]
      , map fst $ allSwapSegment problem
      ]

testProblem :: Problem
testProblem
  = Problem
    { depot = Loc 0 0
    , routes
        = [ []
          , map f [1,2,3]
          , map f [4,5,6]
          , map f [7,8,9]
          , map f [10,11,12]
          ]
    }
  where
    f n = Loc n n

testProblem2 :: Problem
testProblem2
  = Problem
    { depot = Loc 999 999
    , routes
        = [ []
          , map f [0..9]
          , map f [10..19]
          ]
    }
  where
    f n = Loc n n

--------------------------------------------------------------------------------
-- Optimizer
--------------------------------------------------------------------------------

localSearch :: Problem -> [Problem]
localSearch problem
  | x' < x
    = problem : localSearch problem'
  | otherwise
    = [problem]
  where
    x = evaluate problem
    (x', problem') = best (allMutations problem)