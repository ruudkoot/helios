module Helios.Data.Calendar.Types
  ( Date(..)
  , fromJulianDay
  , fromModifiedJulianDay
  , modifiedJulianDayToJulianDay
  ) where

newtype Date = Date { toJulianDay :: Int }
  deriving ( Eq, Ord )

fromJulianDay :: Int -> Date
fromJulianDay = Date

fromModifiedJulianDay :: Int -> Date
fromModifiedJulianDay = fromJulianDay . modifiedJulianDayToJulianDay

-- We should add 2400000.5 as MDJ starts at midnight (0:00) while JD starts at
-- noon (12:00).
modifiedJulianDayToJulianDay :: Int -> Int
modifiedJulianDayToJulianDay mjd = mjd + 2400001
