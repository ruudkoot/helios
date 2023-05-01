module Helios.Optimize.VehicleRouting.Types
( Loc(..)
, Problem(..)
) where

data Loc
  = Loc !Int !Int
  deriving (Eq, Ord)

instance Show Loc where
  show (Loc x y) = show (x, y)

data Problem
  = Problem
    { depot :: Loc
    , routes :: [[Loc]]
    }
  deriving (Eq, Ord, Show)