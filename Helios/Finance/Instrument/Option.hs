{-# LANGUAGE RecordWildCards #-}
module Helios.Finance.Instrument.Option
( Exercise(..)
, Payoff(..)
, Option(..)
, value
, main
) where

import           Helios
import qualified Helios.Data.BinomialTree as BT
import           Helios.Finance.Market

--------------------------------------------------------------------------------
-- Instrument
--------------------------------------------------------------------------------

data Exercise
  = European
  | American
  deriving (Eq, Show)

data Payoff
  = Call
  | Put
  deriving (Eq, Show)

data Option
  = Option
    { exercise :: Exercise
    , payoff :: Payoff
    , strike :: Double
    , expiry :: Double
    }
  deriving (Show)

--------------------------------------------------------------------------------
-- Binomial tree pricer
--------------------------------------------------------------------------------

data Config
  = Config
    { steps :: Int
    }
  deriving (Show)

instance Default Config where
  def = Config { steps = 252 }

data Value
  = Value
    { value :: Double
    , delta :: Double
    }
  deriving (Show)

pricer :: Config -> Market -> Option -> Value
pricer Config{..} Market{..} Option{..}
  = let
      spots   = BT.take (steps + 1) (BT.unfold (u *) (v *) spot)
      payoffs = BT.map (payoff' payoff strike) spots
      values  = BT.backward (value' exercise p' df) payoffs
      deltas  = BT.forward delta' (BT.zip values spots)
    in
      Value { value = BT.head values, delta = BT.head deltas }
  where
    dt = expiry / fromIntegral steps
    u  = 1 + vol * sqrt dt
    v  = 1 - vol * sqrt dt
    p' = 0.5 + rate * sqrt dt / (2 * vol)
    df = 1 / (1 + rate * dt)

payoff' :: Payoff -> Double -> Double -> Double
payoff' Call strike spot = max 0 (spot - strike)
payoff' Put  strike spot = max 0 (strike - spot)

value' :: Exercise -> Double -> Double -> Double -> Double -> Double -> Double
value' European p' df _  p1 p2 =         df * (p' * p1 + (1 - p') * p2)
value' American p' df p0 p1 p2 = max p0 (df * (p' * p1 + (1 - p') * p2))

delta' :: a -> (Double, Double) -> (Double, Double) -> Double
delta' _ (v1,s1) (v2,s2) = (v1 - v2) / (s1 - s2)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  let mkt = Market 0.05 100 0.20
  print $ pricer def mkt (Option European Call 100 1)
  print $ pricer def mkt (Option European Put  100 1)
  print $ pricer def mkt (Option American Call 100 1)
  print $ pricer def mkt (Option American Put  100 1)