{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module Helios.Finance.Instrument
( Instrument(..)
, Instrument'
, instrument'
) where

import Helios.Finance.Market
import Helios.Finance.Price

class Show a => Instrument a where
  price :: Market -> a -> Taylor

data Instrument'
  = forall i. Instrument i => Instrument' i

deriving instance Show Instrument'

instrument' :: Instrument i => i -> Instrument'
instrument' = Instrument'
