{-# LANGUAGE RecordWildCards #-}
module Helios.Finance.Instrument.Option
( Exercise(..)
, Payoff(..)
, Option(..)
, theta
) where

import           Helios
import qualified Helios.Data.BinomialTree as BT
import           Helios.Finance.Instrument
import           Helios.Finance.Market
import           Helios.Finance.Price

--------------------------------------------------------------------------------
-- Instrument
--------------------------------------------------------------------------------

data Exercise
  = European
  | American
  deriving (Eq, Ord, Show)

data Payoff
  = Call
  | Put
  deriving (Eq, Ord, Show)

data Option
  = Option
    { exercise :: Exercise
    , payoff :: Payoff
    , strike :: Double
    , expiry :: Double
    }
  deriving (Eq, Ord, Show)

instance Instrument Option where
  price = pricer def

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

pricer :: Config -> Market -> Option -> Taylor
pricer Config{..} Market{..} Option{..}
  = let
      spots   = BT.take (steps + 1) (BT.unfold (u *) (v *) spot)
      payoffs = BT.map (payoff' payoff strike) spots
      values  = BT.backward (value' exercise p' df) payoffs
      greeks  = iterate (\g -> BT.forward derivative (BT.zip g spots)) values
    in
      Taylor (map BT.head greeks)
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

derivative :: a -> (Double, Double) -> (Double, Double) -> Double
derivative _ (v1,s1) (v2,s2) = (v1 - v2) / (s1 - s2)

--------------------------------------------------------------------------------
-- Risk
--------------------------------------------------------------------------------

-- FIXME: move pricing date, not expiry date
theta :: Market -> Option -> Double
theta market option
  = value (pricer def market option') - value (pricer def market option)
  where
    option' = option { expiry = expiry option - (1/252) }
