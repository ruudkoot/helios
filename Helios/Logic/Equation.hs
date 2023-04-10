{-# LANGUAGE OverloadedStrings #-}

module Helios.Logic.Equation where

import Helios.Logic.Term

--------------------------------------------------------------------------------
-- Equations
--------------------------------------------------------------------------------

data Equation sig = Term sig :=: Term sig

instance (SIG sig) => Show (Equation sig) where
  show (x :=: y) = show x ++ "=" ++ show y

--------------------------------------------------------------------------------
-- Addition
--------------------------------------------------------------------------------

data ADDITION

sigAddition :: Sig ADDITION
sigAddition =
  signature
  [ "0"
  , "s_"
  , "_+_"
  ]

instance SIG ADDITION where
  _SIG_ = sigAddition

eqAddition :: [Equation ADDITION]
eqAddition =
  [ "X+0" :=: "X"
  , "X+s(Y)" :=: "s(X+Y)"
  ]

-- TODO: prove s(0)+s(s(0))=s(s(s(0)))

--------------------------------------------------------------------------------
-- Symbolic differentiation
--------------------------------------------------------------------------------

data SYMDIFF

sigSymDiff :: Sig SYMDIFF
sigSymDiff =
  signature
  [ "0"
  , "1"
  , "dx_"
  , "_*_"
  , "_+_"
  ]

instance SIG SYMDIFF where
  _SIG_ = sigSymDiff

eqSymDiff :: [Equation SYMDIFF]
eqSymDiff =
  [ "dx(x)" :=: "1"
  , "dx(y)" :=: "0"
  , "dx(U+V)" :=: "dx(U)+dx(V)"
  , "dx(U*V)" :=: "(U*dx(V))+(dx(U)*V)"
  ]

--------------------------------------------------------------------------------
-- Group theory
--------------------------------------------------------------------------------

data GROUP

sigGroup :: Sig GROUP
sigGroup =
  signature
  [ {-"e"
  , -}"i_"
  , "_._"
  ]

instance SIG GROUP where
  _SIG_ = sigGroup

eqGroup :: [Equation GROUP]
eqGroup =
  [ "(X.Y).Z" :=: "X.(Y.Z)"
  , "e.X" :=: "X"
  , "iX.X" :=: "e"
  ]

-- TODO: prove e=X.iX

