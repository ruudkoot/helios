module Helios.Finance.Price
( Taylor(..)
, presentValue
, delta
, gamma
, speed
) where

newtype Taylor
  = Taylor [Double]
  deriving (Show)

presentValue, delta, gamma, speed :: Taylor -> Double
presentValue (Taylor ts) = ts !! 0
delta (Taylor ts) = ts !! 1
gamma (Taylor ts) = ts !! 2
speed (Taylor ts) = ts !! 3