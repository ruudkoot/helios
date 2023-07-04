{-# LANGUAGE RecordWildCards #-}
module BasketCDS where

import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import           Data.Maybe

--------------------------------------------------------------------------------
-- Base types
--------------------------------------------------------------------------------

newtype Tenor = Y Double
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Rate curves
--------------------------------------------------------------------------------

data RateCurve
  = RateCurve { pillars :: Map Tenor Double }
  deriving (Eq, Show)

-- FIXME: we do not interpolate at all
-- FIXME: log interpolation instead of linear?
df :: RateCurve -> Tenor -> Double
df RateCurve{..} tenor
  = fromMaybe (error "df: tenor does not fall on a pillar")
  $ Map.lookup tenor pillars

-- FIXME: rates instead of discount factors
rcUSD :: RateCurve
rcUSD
  = RateCurve $ Map.fromList
    [ (Y 0.00, 1.00)
    , (Y 0.25, 0.99)
    , (Y 0.50, 0.98)
    , (Y 0.75, 0.97)
    , (Y 1.00, 0.96)
    , (Y 1.25, 0.95)
    , (Y 1.50, 0.94)
    , (Y 1.75, 0.93)
    , (Y 2.00, 0.92)
    , (Y 2.25, 0.91)
    , (Y 2.50, 0.90)
    , (Y 2.75, 0.89)
    , (Y 3.00, 0.88)
    , (Y 3.25, 0.87)
    , (Y 3.50, 0.86)
    , (Y 3.75, 0.85)
    , (Y 4.00, 0.84)
    , (Y 4.25, 0.83)
    , (Y 4.50, 0.82)
    , (Y 4.75, 0.81)
    , (Y 5.00, 0.80)
    ]

--------------------------------------------------------------------------------
-- Single-name credit spreads
--------------------------------------------------------------------------------


