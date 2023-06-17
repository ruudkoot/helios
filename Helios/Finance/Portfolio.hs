module Helios.Finance.Portfolio
( PortfolioEntry(..)
, Portfolio(..)
) where

import Helios.Finance.Instrument

--------------------------------------------------------------------------------
-- Portfolios
--------------------------------------------------------------------------------

data PortfolioEntry
  = PortfolioEntry
    { instrument :: Instrument'
    , quantity :: Double
    }
  deriving (Show)

portfolioEntry :: Instrument i => i -> Double -> PortfolioEntry
portfolioEntry i q
  = PortfolioEntry (instrument' i) q

data Portfolio
  = Portfolio [PortfolioEntry]
  deriving (Show)
