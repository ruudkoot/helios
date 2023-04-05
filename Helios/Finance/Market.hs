module Helios.Finance.Market
( Market(..)
) where

data Market
  = Market
    { rate :: Double
    , spot :: Double
    , vol :: Double
    }
  deriving (Eq, Show)