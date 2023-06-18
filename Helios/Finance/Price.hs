module Helios.Finance.Price
( Taylor(..)
, presentValue
, delta
, gamma
, speed
, sumTaylor
, scaleTaylor
) where

import Helios.Data.List ( zipListWith )

newtype Taylor
  = Taylor { taylor :: [Double] }
  deriving (Show)

presentValue, delta, gamma, speed :: Taylor -> Double
presentValue (Taylor ts) = ts !! 0
delta (Taylor ts) = ts !! 1
gamma (Taylor ts) = ts !! 2
speed (Taylor ts) = ts !! 3

-- FIXME: Monoid? Num?
sumTaylor :: [Taylor] -> Taylor
sumTaylor = Taylor . zipListWith sum . map taylor

scaleTaylor :: Double -> Taylor -> Taylor
scaleTaylor q (Taylor xs) = Taylor (map (q*) xs)