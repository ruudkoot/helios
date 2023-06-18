{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
module Helios.Finance.Portfolio
( Portfolio
, long, long'
, short, short'
) where

import Helios.Finance.Instrument
import Helios.Finance.Market
import Helios.Finance.Price

--------------------------------------------------------------------------------
-- Packet
--------------------------------------------------------------------------------

data Packet
  = Packet
    { quantity :: Double
    , instrument :: Instrument'
    }
  deriving (Show)

instance Instrument Packet where
  price market (Packet q i)
    = scaleTaylor q (price market i)

packet
  :: Instrument i => Double -> i -> Packet
packet q i
  = Packet q (instrument' i)

--------------------------------------------------------------------------------
-- Portfolio
--------------------------------------------------------------------------------

data Portfolio
  = Portfolio [Packet]
  deriving (Show)

instance Semigroup Portfolio where
  Portfolio p1 <> Portfolio p2
    = Portfolio (p1 <> p2)

instance Monoid Portfolio where
  mempty
    = Portfolio mempty

long
  :: Instrument i => i -> Portfolio
long i
  = long' 1 i

long'
  :: Instrument i => Double -> i -> Portfolio
long' n i
  = Portfolio [packet n i]

short
  :: Instrument i => i -> Portfolio
short i
  = short' 1 i

short'
  :: Instrument i => Double -> i -> Portfolio
short' n i
  = Portfolio [packet (-n) i]

instance Instrument Portfolio where
  price market (Portfolio ps)
    = sumTaylor (map (price market) ps)