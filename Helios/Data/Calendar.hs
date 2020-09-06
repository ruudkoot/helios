-- ISO 8601
-- RFC 3339

module Helios.Data.Calendar
  ( Types.Date
  , Gregorian.Month(..)
  , Gregorian.Weekday(..)
  , Gregorian.weekday
  , date
  , showDate
  , today
  ) where

import qualified Data.Time

import qualified Helios.Data.Calendar.Types as Types
import qualified Helios.Data.Calendar.Gregorian as Gregorian

--------------------------------------------------------------------------------
-- * Gregorian
--------------------------------------------------------------------------------

date :: Int -> Gregorian.Month -> Int -> Types.Date
date y m d =
  Gregorian.toJulianDayNumber $
    Gregorian.date (Gregorian.Year y) m (Gregorian.Day d)

showDate :: Types.Date -> String
showDate = Gregorian.prettyDate . Gregorian.fromJulianDayNumber

instance Show Types.Date where
  show = showDate


--------------------------------------------------------------------------------
-- * Data.Time
--------------------------------------------------------------------------------

today :: IO Types.Date
today = fromUTCTime <$> Data.Time.getCurrentTime

fromUTCTime :: Data.Time.UTCTime -> Types.Date
fromUTCTime
  = Types.fromModifiedJulianDay
  . fromInteger
  . Data.Time.toModifiedJulianDay
  . Data.Time.utctDay
