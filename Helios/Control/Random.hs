{-# LANGUAGE ScopedTypeVariables #-}
module Helios.Control.Random
( irwinHall
, fisherYates
) where

import           System.Random

import           Helios.Control.Monad
import qualified Helios.Data.Array    as Array

--------------------------------------------------------------------------------
-- Standard normal distribution N(0,1)
--------------------------------------------------------------------------------

-- Irwin-Hall distribution with range (-6, 6).
irwinHall :: IO Double
irwinHall = fmap (\us -> sum us - 6) (replicateM 12 randomIO)

boxMuller :: IO Double
boxMuller = error "boxMuller"

--------------------------------------------------------------------------------
-- Permutations
--------------------------------------------------------------------------------

fisherYates :: [a] -> IO [a]
fisherYates xs = do
  let n = length xs
  let arr = Array.listArray (0, n-1) xs
  arr <- forAccumM_ arr [0 .. n-2] $ \arr i -> do
    j <- randomRIO (i, n-1)
    return (Array.exchange i j arr)
  return (Array.elems arr)