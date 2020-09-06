module Helios.Math
  ( (/!)
  ) where

-- | Integer division, round to zero.
(/!) :: Integral a => a -> a -> a
n /! m = signum n * (abs n `div` m)
