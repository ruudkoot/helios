{-# LANGUAGE RecordWildCards #-}
module Helios.Finance.Instrument.EuropeanOption
( Payoff(..)
, EuropeanOption(..)
, value
, main
) where

import Helios.Data.List ( isSingleton, slide )
import Helios.Finance.Market

data Payoff
  = Call
  | Put
  deriving (Eq, Show)

data EuropeanOption
  = EuropeanOption
    { strike :: Double
    , payoff :: Payoff
    , expiry :: Double
    }
  deriving (Show)

data Config
  = Config
    { steps :: Int
    }
  deriving (Show)

value :: Config -> Market -> EuropeanOption -> Double
value Config{..} Market{..} EuropeanOption{..}
  = let
      st = take (steps + 1) (iterate (\ss@(s:_) -> u * s : map (v *) ss) [spot])
      pt = map (map (\spot -> payout payoff spot strike)) st
      vt = until isSingleton (slide (\p1 p2 -> df * (p' * p1 + (1 - p') * p2))) (last pt)
    in
      head vt
  where
    ts = expiry / fromIntegral steps
    u  = 1 + vol * sqrt ts
    v  = 1 - vol * sqrt ts
    p' = 0.5 + rate * sqrt ts / (2 * vol)
    df = 1 / (1 + rate * ts)

payout :: Payoff -> Double -> Double -> Double
payout Call spot strike = max 0 (spot - strike)
payout Put  spot strike = max 0 (strike - spot)

main :: IO ()
main = do
  print $ value (Config 4) (Market 0.05 100 0.20) (EuropeanOption 100 Call 1)