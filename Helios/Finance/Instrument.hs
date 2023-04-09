module Helios.Finance.Instrument
( Instrument(..)
) where

import Helios.Finance.Market
import Helios.Finance.Price

class Instrument a where
  price :: Market -> a -> Taylor
