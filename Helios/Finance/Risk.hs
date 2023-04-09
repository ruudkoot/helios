module Helios.Finance.Risk
( vega
, rho
) where

import Helios.Finance.Instrument
import Helios.Finance.Market
import Helios.Finance.Price

vega :: Instrument a => Market -> a -> Double
vega market instrument
  = (value (price market1 instrument) - value (price market2 instrument)) / 2
  where
    market1 = market { vol = vol market + 0.01 }
    market2 = market { vol = vol market - 0.01 }

rho :: Instrument a => Market -> a -> Double
rho market instrument
  = (value (price market1 instrument) - value (price market2 instrument)) / 2
  where
    market1 = market { rate = rate market + 0.01 }
    market2 = market { rate = rate market - 0.01 }
