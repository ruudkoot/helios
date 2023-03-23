{-# LANGUAGE NamedFieldPuns, RecordWildCards, ViewPatterns #-}
module Helios.Search.Graph.Example.VacuumWorld
( Location(..)
, State(..)
, Environment(..)
, Action(..)
, act
, problem
) where

import Helios.Search.Graph

data Location = A | B
  deriving (Bounded, Enum, Eq, Show)

data State = Clean | Dirty
  deriving (Bounded, Enum, Eq, Show)

data Environment
  = Environment
    { location :: !Location
    , a :: !State
    , b :: !State
    }
  deriving (Bounded, Eq, Show)

-- FIMXE: Helios.Prelude
instance Enum Environment where
  fromEnum Environment{..}
    = fromEnum location * 4 + fromEnum a * 2 + fromEnum b
  toEnum n0
    = Environment{..}
    where
      (toEnum -> location, n1)
        = n0 `divMod` 4
      (toEnum -> a, n2)
        = n1 `divMod` 2
      b = toEnum n2

data Action = NoOp | GoLeft | GoRight | Suck
  deriving (Eq, Show)

act :: Environment -> Action -> Environment
act environment NoOp
  = environment
act environment GoLeft
  = environment { location = A }
act environment GoRight
  = environment { location = B }
act environment Suck
  = case location environment of
      A -> environment { a = Clean }
      B -> environment { b = Clean }

problem :: Problem Action Environment
problem
  = Problem
    { initialState
        = Environment{ location = A, a = Dirty, b = Dirty }
    , successorFn
        = \ environment ->
            map (\action -> (action, act environment action))
                [NoOp, GoLeft, GoRight, Suck]
    , goalTest
        = \ environment ->
            case environment of
              Environment{ a = Clean, b = Clean } -> True
              _ -> False
    , stepCost
        = \ _ _ _ -> 1
    }
