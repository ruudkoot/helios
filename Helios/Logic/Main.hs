{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Helios.Logic.Main where

import Helios.Logic.Term
import Helios.Logic.Rule

import Helios.Logic.Sig.CATSYL ()
import Helios.Logic.Sig.PROPI

-- TODO: parse longer variable and constant names
-- TODO: environments for variables and uninterpreted constants

--------------------------------------------------------------------------------
-- Propositional implicational logic
--------------------------------------------------------------------------------

term1, term2, term3 :: Term PROPI
term1 = "P=>Q"
term2 = "~R=>(S=>T)"
term3 = "~(A=>B)"

rules_2_1_2b :: [Rule (Term PROPI)]
rules_2_1_2b =
  [ ["a","e"] :> "d"
  , ["e","~c"] :> "a"
  , ["d","~c","e"] :> "~f"
  , ["~f"] :> "b"
  , ["a","b"] :> "t"
  ]

axioms_2_1_2b :: [Deriv (Term PROPI)]
axioms_2_1_2b =
  fmap axiom
  [ "~c"
  , "e"
  ]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

data Verbosity
  = Silent
  | Verbose
  deriving ( Eq, Show )

prove :: Verbosity -> Prove sig -> [Deriv (Term sig)]
prove verbosity Prove{..} =
  (if verbosity == Silent then filter ((==toProve) . conclusion) else id)
    (forwardChain rules (map axiom assumptions ++ axioms))

data Prove sig =
  Prove
  { toProve :: Term sig
  , rules :: [Rule (Term sig)]
  , assumptions :: [Term sig]
  , axioms :: [Deriv (Term sig)]
  } deriving ( Show )

proveRefl :: Prove PROPI
proveRefl =
  Prove
  { toProve     = "p=>p"
  , rules       = [modusPonens]
  , assumptions = []
  , axioms      = hilbertAxioms ["p"]
  }

proveRefl' :: Prove PROPI
proveRefl' =
  proveRefl
  { axioms =
    map axiom
    ["(p=>((p=>p)=>p))=>((p=>(p=>p))=>(p=>p))"
    ,"p=>((p=>p)=>p)"
    ,"p=>(p=>p)"
    ]
  }

prove2_1_1 :: Prove PROPI
prove2_1_1 =
  Prove
  { toProve     = "p=>r"
  , rules       = [modusPonens]
  , assumptions = ["p=>q","q=>r"]
  , axioms      = hilbertAxioms ["p","q","r'"]
  }

prove2_1_1' :: Prove PROPI
prove2_1_1' =
  prove2_1_1
  { axioms =
    map axiom
    ["(q=>r)=>(p=>(q=>r))"
    ,"(p=>(q=>r))=>((p=>q)=>(p=>r))"
    ]
  }

main :: IO ()
main = mapM_ (putStrLn . show) (prove Verbose proveRefl')
