{-# LANGUAGE ViewPatterns #-}

module Helios.Data.Calendar.Gregorian
  ( Year(..)
  , Era(..)
  , isCommonEra
  , isBeforeCommonEra
  , isLeapYear
  , Month(..)
  , toMonthNumber
  , fromMonthNumber
  , daysInMonth
  , Day(..)
  , Weekday(..)
  , weekday
  , Date(year, month, day)
  , isValidDate
  , isProleptic
  , date
  , prolepticDate
  , toJulianDayNumber
  , fromJulianDayNumber
  , prettyDate
  ) where

import qualified Helios.Data.Calendar.Types as Types
import           Helios.Data.String
import           Helios.Math

--------------------------------------------------------------------------------
-- * Years
--------------------------------------------------------------------------------

newtype Year = Year { astronomicalYearNumber :: Int }
  deriving (Eq, Ord)

isCommonEra :: Year -> Bool
isCommonEra (Year y) =
  y >= 1

isBeforeCommonEra :: Year -> Bool
isBeforeCommonEra = not . isCommonEra

isLeapYear :: Year -> Bool
isLeapYear (Year y) =
  y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)

data Era = BCE Int | CE Int
  deriving (Eq)

instance Ord Era where
  BCE _ <= CE  _ = True
  CE  _ <= BCE _ = False
  BCE x <= BCE y = x >= y
  CE  x <= CE  y = x <= y

eraToYear :: Era -> Year
eraToYear (CE y) = Year y
eraToYear (BCE y) = Year (-y - 1)

yearToEra :: Year -> Era
yearToEra (Year y)
  | y >= 1    = CE y
  | otherwise = BCE (-y + 1)

--------------------------------------------------------------------------------
-- * Months
--------------------------------------------------------------------------------

data Month
  = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Bounded, Enum, Eq, Ord, Show)

toMonthNumber :: Month -> Int
toMonthNumber = succ . fromEnum

fromMonthNumber :: Int -> Month
fromMonthNumber m
  | m >= 1, m <= 12 = toEnum (pred m)
  | otherwise       = errorMonthNumber m

daysInMonth :: Year -> Month -> Int
daysInMonth _ Jan = 31
daysInMonth y Feb
  | isLeapYear y  = 29
  | otherwise     = 28
daysInMonth _ Mar = 31
daysInMonth _ Apr = 30
daysInMonth _ May = 31
daysInMonth _ Jun = 30
daysInMonth _ Jul = 31
daysInMonth _ Aug = 31
daysInMonth _ Sep = 30
daysInMonth _ Oct = 31
daysInMonth _ Nov = 30
daysInMonth _ Dec = 31

--------------------------------------------------------------------------------
-- * Days
--------------------------------------------------------------------------------

newtype Day = Day Int
  deriving (Eq, Ord)

isValidDay :: Year -> Month -> Day -> Bool
isValidDay y m (Day d) =
  d >= 1 && d <= daysInMonth y m

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Enum, Show)

weekday :: Types.Date -> Weekday
weekday date = toEnum (Types.toJulianDay date `mod` 7)

--------------------------------------------------------------------------------
-- * Dates
--------------------------------------------------------------------------------

data Date = Date { year :: !Year, month :: !Month, day :: !Day }

isValidDate :: Year -> Month -> Day -> Bool
isValidDate y m d =
  isValidDay y m d

isProleptic :: Year -> Month -> Day -> Bool
isProleptic y m d =
  (y, m, d) < (Year 1592, Oct, Day 15)

date :: Year -> Month -> Day -> Date
date y m d
  | not (isValidDate y m d) = errorInvalid y m d
  | isProleptic y m d       = errorProleptic y m d
  | otherwise               = Date y m d

prolepticDate :: Year -> Month -> Day -> Date
prolepticDate y m d
  | not (isValidDate y m d) = errorInvalid y m d
  | isBeforeCommonEra y     = errorBeforeCommonEra y m d
  | otherwise               = Date y m d

historicDate :: Era -> Month -> Day -> Date
historicDate e m d
  | not (isValidDate y m d) = errorInvalid y m d
  | otherwise               = Date y m d
  where y = eraToYear e

toJulianDayNumber :: Date -> Types.Date
toJulianDayNumber (Date (Year y) (toMonthNumber -> m) (Day d)) =
    Types.Date jdn
  where jdn = (1461 * (y + 4800 + (m - 14)/!12))/!4
                +
              (367 * (m - 2 - 12 * ((m - 14)/!12)))/!12
                -
              (3 * ((y + 4900 + (m - 14)/!12)/!100))/!4
                +
              d - 32075

fromJulianDayNumber :: Types.Date -> Date
fromJulianDayNumber (Types.Date jdn) =
  let y = 4716
      j = 1401
      m = 2
      n = 12
      r = 4
      p = 1461
      v = 3
      u = 5
      s = 153
      w = 2
      b = 274377
      c = -38
      f = jdn + j + (((4 * jdn + b) `div` 146097) * 3) `div` 4 + c
      e = r * f + v
      g = (e `mod` p) `div` r
      h = u * g + w
      day = (h `mod` s) `div` u + 1
      month = (h `div` s + m) `mod` n + 1
      year = (e `div` p) - y + (n + m - month) `div` n
  in Date (Year year) (fromMonthNumber month) (Day day)


--------------------------------------------------------------------------------
-- * Formatting
--------------------------------------------------------------------------------

prettyDay :: Day -> String
prettyDay (Day d) = padRight '0' 2 (show d)

prettyMonth :: Month -> String
prettyMonth = show

prettyYear :: Year -> String
prettyYear (Year y)
  | y >= 0    = padRight '0' 4 (show y)
  | y >= -999 = "-" ++ padRight '0' 3 (show (-y))
  | otherwise = show y

prettyDate :: Date -> String
prettyDate (Date y m d) =
  prettyDay d ++ "-" ++ prettyMonth m ++ "-" ++ prettyYear y


--------------------------------------------------------------------------------
-- * Errors
--------------------------------------------------------------------------------

showYMD (Year y) m (Day d) =
  "'Y" ++ show y ++ "-" ++ show m ++ "-D" ++ show d ++ "'"

errorMonthNumber m =
  error (show m ++ " is not a valid month number (1 ≤ M ≤ 12).")

errorProleptic y m d =
  error (showYMD y m d ++ " is a proleptic Gregorian date.")

errorBeforeCommonEra y m d =
  error (showYMD y m d ++ " is a Gregorian date before the Common Era.")

errorInvalid  y m d =
  error (showYMD y m d ++ " is not a valid Gregorian date.")
