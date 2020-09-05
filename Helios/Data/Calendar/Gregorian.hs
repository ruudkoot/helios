module Helios.Data.Calendar.Gregorian
  ( Year(..)
  , Era(..)
  , isCommonEra
  , isLeapYear
  , Month(..)
  , dayCount
  , Day(..)
  , Date(year, month, day)
  , isValidDate
  , isProleptic
  , date
  , prolepticDate
  ) where

import qualified Helios.Data.Calendar.Types as Types

--------------------------------------------------------------------------------
-- * Years
--------------------------------------------------------------------------------

newtype Year = Year { astronomicalYearNumber :: Int }

data Era = CE Int | BCE Int

fromEra :: Era -> Year
fromEra

toEra :: Year -> Era
toEra (Year y)
  | y >= 1    = CE y
  | otherwise = BCE (-y + 1)

isCommonEra :: Year -> Bool
isCommonEra (Year y) =
  y >= 1

isLeapYear :: Year -> Bool
isLeapYear (Year y) =
  y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)


--------------------------------------------------------------------------------
-- * Months
--------------------------------------------------------------------------------

data Month
  = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Eq, Ord, Show)

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

isValidDay :: Year -> Month -> Day -> Bool
isValidDay y m (Day d) =
  d >= 1 && d <= daysInMonth y m

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

--------------------------------------------------------------------------------
-- * Dates
--------------------------------------------------------------------------------

data Date = Date { year :: Year, month :: Month, day :: Day }

isValidDate :: Year -> Month -> Day -> Bool
isValidDate y m d =
  isValidDay y m d

isProleptic :: Year -> Month -> Day -> Bool
isProleptic (Year y) m (Day d) =


date :: Year -> Month -> Day -> Date
date y m d
  | isProleptic y m d = errorProleptic y m d
  | isValidDate y m d = Date y m d
  | othwerwise        = errorInvalid y m d

prolepticDate :: Year -> Month -> Day -> Date
prolepticDate y m d
  |
  | isValidDate y m d = Date y m d
  | othwerwise        = errorInvalid y m d

--------------------------------------------------------------------------------
-- * Errors
--------------------------------------------------------------------------------

showYMD (Year y) m (Day d) =
  "'" ++ show y ++ "-" ++ show m ++ "-" ++ show d ++ "'"

errorProleptic y m d =
  error (showYMD y m d ++ " is a proleptic Gregorian date.")

errorInvalid  y m d =
  error (showYMD y m d ++ " is not a valid Gregorian date.")
