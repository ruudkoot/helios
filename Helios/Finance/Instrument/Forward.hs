{-# LANGUAGE RecordWildCards #-}
module Helios.Finance.Instrument.Forward
( Forward(..)
) where

import           Helios.Finance.Instrument
import           Helios.Finance.Market
import           Helios.Finance.Price
import           Helios.Finance.Rates

--------------------------------------------------------------------------------
-- Instrument
--------------------------------------------------------------------------------

data Forward
  = Forward
    { forward :: Double
    , expiry :: Double
    }
  deriving (Eq, Ord, Show)

instance Instrument Forward where
  price = pricer

--------------------------------------------------------------------------------
-- Pricer
--------------------------------------------------------------------------------

pricer :: Market -> Forward -> Taylor
pricer Market{..} Forward{..}
  = Taylor [value, delta]
  where
    value = spot - df rate expiry * forward
    delta = 1.0