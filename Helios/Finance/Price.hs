module Helios.Finance.Price
( Taylor(..)
, value
, delta
, gamma
, speed
) where

newtype Taylor
  = Taylor [Double]
  deriving (Show)

value, delta, gamma, speed :: Taylor -> Double
value (Taylor ts) = ts !! 0
delta (Taylor ts) = ts !! 1
gamma (Taylor ts) = ts !! 2
speed (Taylor ts) = ts !! 3