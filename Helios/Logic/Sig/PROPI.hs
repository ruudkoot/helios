{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Helios.Logic.Sig.PROPI
  ( PROPI
  , modusPonens
  , hilbertAxioms
  , p1
  ) where

import           Data.List ( nub )
import qualified Data.Map.Strict as Map

import Helios.Logic.Infinite
import Helios.Logic.Term
import Helios.Logic.Rule

--------------------------------------------------------------------------------
-- Propositional implicational logic
--------------------------------------------------------------------------------

data PROPI

propI :: Sig PROPI
propI =
  signature
  [ "~_"
  , "_=>_"
  ]

instance SIG PROPI where
  _SIG_ = propI

modusPonens :: Rule (Term PROPI)
modusPonens = ["φ", "φ=>ψ"] :> "ψ"

hilbertAxioms :: [String] -> [Deriv (Term PROPI)]
hilbertAxioms consts =
  axiomSchema consts
  [ "φ=>(ψ=>φ)"
  , "(φ=>(ψ=>χ))=>((φ=>ψ)=>(φ=>χ))"
  , "(~φ=>~ψ)=>(ψ=>φ)"
  ]

p1 :: Term PROPI
p1 = "φ=>φ"

-- FIXME: quickMerge -->
axiomSchema :: SIG sig => [String] -> [Term sig] -> [Deriv (Term sig)]
axiomSchema consts = map axiom . quickMerge . map (instSchema consts)
  where quickMerge [a:as,b:bs,c:cs] = [a,b,c] ++ quickMerge [as,bs,cs]
        quickMerge _ = error "quickMerge"

-- FIXME: not unique due to 'cross' (... nub)
-- FIXME: infiniteCross . replicate -> infinitePower
instSchema :: forall sig. SIG sig => [String] -> Term sig -> [Term sig]
instSchema consts t = nub (map (\u -> apply u t) us)
  where vs = vars t
        ts = concat (groundTerms consts) :: [Term sig]
        ws = infiniteCross (replicate (length vs) ts)
        us = map (mkUnif vs) ws

mkUnif :: [String] -> [Term sig] -> Unif sig
mkUnif vs ts = Unif (Map.fromList (zip vs ts))
