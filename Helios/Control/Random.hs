module Helios.Control.Random
( irwinHall
) where

import Control.Monad
import System.Random

--------------------------------------------------------------------------------
-- Standard normal distribution N(0,1)
--------------------------------------------------------------------------------

-- Irwin-Hall distribution with range (-6, 6).
irwinHall :: IO Double
irwinHall = fmap (\us -> sum us - 6) (replicateM 12 randomIO)

boxMuller :: IO Double
boxMuller = undefined
