module Helios.Data.Temporal
  ( Date
  , Month
  ) where

--------------------------------------------------------------------------------
-- * Gregorian calendar
--------------------------------------------------------------------------------

data Month
  = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Eq, Ord, Show)


