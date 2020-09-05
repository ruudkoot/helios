module Helios.Data.Calendar.Types
  ( Date(..)
  ) where

newtype Date = Date { julianDayNumber :: Int }
