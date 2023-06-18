{-# LANGUAGE ScopedTypeVariables #-}
module Helios.Control.Random
( irwinHall
, standardNormal
, fisherYates
, partitionU
, main
) where

import           System.Random

import           Helios.Control.Monad
import qualified Helios.Data.Array              as Array
import qualified Helios.Data.List               as List
import qualified Helios.Math.NormalDistribution as NormalDistribution

--------------------------------------------------------------------------------
-- Standard normal distribution N(0,1)
--------------------------------------------------------------------------------

-- Irwin-Hall distribution with range (-6, 6).
-- NOTE: standardNormal is faster!
irwinHall :: IO Double
irwinHall = fmap (\us -> sum us - 6) (replicateM 12 randomIO)

boxMuller :: IO Double
boxMuller = error "boxMuller"

standardNormal :: IO Double
standardNormal = NormalDistribution.inverseCDF <$> randomIO

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

--------------------------------------------------------------------------------
-- Partitions
--------------------------------------------------------------------------------

partitionU :: Int -> [a] -> IO [[a]]
partitionU n xs = do
  ps <- replicateM (length xs) (randomRIO (0, n-1))
  return $ map reverse (partitionU' (replicate n []) (zip ps xs))

partitionU' :: [[a]] -> [(Int,a)] -> [[a]]
partitionU' ass []
  = ass
partitionU' ass ((p,x):pxs)
  = partitionU' (List.updateAt p (x:) ass) pxs

--------------------------------------------------------------------------------
-- Debug
--------------------------------------------------------------------------------

main :: IO ()
main = putStrLn =<< unlines . map show <$> replicateM 1000000 standardNormal