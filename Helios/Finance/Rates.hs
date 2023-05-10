module Helios.Finance.Rates
( df
) where

-- | Discount factor
df :: Double -> Double -> Double
df r t = exp (-r*t)