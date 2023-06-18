{-# LANGUAGE RecordWildCards #-}
module Helios.Finance.Instrument.Option
( Exercise(..)
, Payoff(..)
, Option(..)
, bullSpread
, bearSpread
, straddle
, strangle
, riskReversal
, butterfly
, condor
, theta
, main
) where

import           Helios
import           Helios.Control.Monad
import qualified Helios.Control.Random      as Random
import qualified Helios.Data.BinomialTree   as BT
import qualified Helios.Data.List           as List
import           Helios.Finance.Instrument
import           Helios.Finance.Market
import           Helios.Finance.Portfolio
import           Helios.Finance.Price
import           Helios.Math.Statistics
import qualified Helios.Text.CSV            as CSV

--------------------------------------------------------------------------------
-- Instrument
--------------------------------------------------------------------------------

-- TODO: Bermudan
data Exercise
  = European
  | American
  deriving (Eq, Ord, Show)

-- TODO: Binary/Digital
data Payoff
  = Call
  | Put
  | BinaryCall
  | BinaryPut
  deriving (Eq, Ord, Show)

payoffFunction :: Payoff -> Double -> Double -> Double
payoffFunction Call       strike spot = max 0 (spot - strike)
payoffFunction Put        strike spot = max 0 (strike - spot)
payoffFunction BinaryCall strike spot = if strike >= spot then 1.0 else 0.0
payoffFunction BinaryPut  strike spot = if strike < spot then 1.0 else 0.0

data Option
  = Option
    { exercise :: Exercise
    , payoff :: Payoff
    , strike :: Double
    , expiry :: Double
    }
  deriving (Eq, Ord, Show)

instance Instrument Option where
  price = pricerBT def

--------------------------------------------------------------------------------
-- Strategy
--------------------------------------------------------------------------------

bullSpread
  :: Double -> Double -> Double -> Portfolio
bullSpread strike1 strike2 expiry
  = long (Option European Call strike1 expiry)
    <>
    short (Option European Call strike2 expiry)

bearSpread
  :: Double -> Double -> Double -> Portfolio
bearSpread strike1 strike2 expiry
  = short (Option European Put strike1 expiry)
    <>
    long (Option European Put strike2 expiry)

straddle
  :: Double -> Double -> Portfolio
straddle strike expiry
  = long (Option European Call strike expiry)
    <>
    long (Option European Put strike expiry)

strangle
  :: Double -> Double -> Double -> Portfolio
strangle strike1 strike2 expiry
  = long (Option European Call strike1 expiry)
    <>
    long (Option European Put strike2 expiry)

riskReversal
  :: Double -> Double -> Double -> Portfolio
riskReversal strike1 strike2 expiry
  = long (Option European Call strike1 expiry)
    <>
    short (Option European Put strike2 expiry)

butterfly
  :: Double -> Double -> Double -> Double -> Portfolio
butterfly strike1 strike2 strike3 expiry
  = condor strike1 strike2 strike2 strike3 expiry

condor
  :: Double -> Double -> Double -> Double -> Double -> Portfolio
condor strike1 strike2 strike3 strike4 expiry
  = long (Option European Call strike1 expiry)
    <>
    short (Option European Call strike2 expiry)
    <>
    short (Option European Call strike3 expiry)
    <>
    long (Option European Call strike4 expiry)

--------------------------------------------------------------------------------
-- Binomial tree pricer
--------------------------------------------------------------------------------

data ConfigBT
  = ConfigBT
    { bt_steps :: Int
    }
  deriving (Show)

instance Default ConfigBT where
  def = ConfigBT { bt_steps = 252 }

pricerBT :: ConfigBT -> Market -> Option -> Taylor
pricerBT ConfigBT{..} Market{..} Option{..}
  = let
      spots   = BT.take (bt_steps + 1) (BT.unfold (u *) (v *) spot)
      payoffs = BT.map (payoffFunction payoff strike) spots
      values  = BT.backward (value exercise p' df) payoffs
      greeks  = iterate (\g -> BT.forward derivative (BT.zip g spots)) values
    in
      Taylor (map BT.head greeks)
  where
    dt = expiry / fromIntegral bt_steps
    u  = 1 + vol * sqrt dt
    v  = 1 - vol * sqrt dt
    p' = 0.5 + rate * sqrt dt / (2 * vol)
    df = 1 / (1 + rate * dt)

value :: Exercise -> Double -> Double -> Double -> Double -> Double -> Double
value European p' df _  p1 p2 =         df * (p' * p1 + (1 - p') * p2)
value American p' df p0 p1 p2 = max p0 (df * (p' * p1 + (1 - p') * p2))

derivative :: a -> (Double, Double) -> (Double, Double) -> Double
derivative _ (v1,s1) (v2,s2) = (v1 - v2) / (s1 - s2)

--------------------------------------------------------------------------------
-- Monte Carlo pricer
--------------------------------------------------------------------------------

data ConfigMC
  = ConfigMC
    { mc_steps :: Int
    , mc_paths :: Int
    }
  deriving (Show)

instance Default ConfigMC where
  def = ConfigMC { mc_steps = 252, mc_paths = 8192 }

-- FIXME: fix random seed
-- FIXME: European only
pricerMT :: ConfigMC -> Market -> Option -> IO Double
pricerMT ConfigMC{..} Market{..} Option{..} = do
  let dt = expiry / fromIntegral mc_steps
  paths <- fmap (concatMap List.unzipL) $
    replicateM ((mc_paths + 1) `div` 2) $
      iterateForM mc_steps (spot, spot) $
        \(spot1, spot2) -> do
          w <- Random.standardNormal
          return
            ( spot1 * (1 + rate * dt + vol * sqrt dt *        w)
            , spot2 * (1 + rate * dt + vol * sqrt dt * negate w)
            )
  let payoffs = map (\path -> payoffFunction payoff strike (last path)) paths
  return (exp (-rate * expiry) * mean payoffs)

--------------------------------------------------------------------------------
-- Risk
--------------------------------------------------------------------------------

-- FIXME: move pricing date, not expiry date
theta :: Market -> Option -> Double
theta market option
  = presentValue (price market option') - presentValue (price market option)
  where
    option' = option { expiry = expiry option - (1/252) }

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  let market = Market 0.05 100 0.20
  let option = Option European Call 100 1
  void $ repeatM $ pricerMT def market option >>= print
