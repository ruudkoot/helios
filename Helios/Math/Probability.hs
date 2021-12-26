module Helios.Math.Probability
(
) where

import Data.Set

newtype Probability
  = Probability { probability :: Double }
  deriving ( Eq, Ord, Num )

data Experiment sampleSpace
  = Experiment
    { probabilityMass :: sampleSpace -> Probability
    }
