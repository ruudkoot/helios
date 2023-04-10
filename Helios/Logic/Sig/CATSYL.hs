{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Helios.Logic.Sig.CATSYL
  ( CATSYL
  , modusBarbara
  ) where

import Helios.Logic.Term
import Helios.Logic.Rule

--------------------------------------------------------------------------------
-- Categorical syllogisms (Aristotle)
--------------------------------------------------------------------------------

data CATSYL

categoricalSyllogism :: Sig CATSYL
categoricalSyllogism =
  signature
  [ "all _ are _"       -- A
  , "no _ are _"        -- E
  , "some _ are _"      -- I
  , "some _ are not _"  -- O
  ]

instance SIG CATSYL where
  _SIG_ = categoricalSyllogism

modusBarbara :: Rule (Term CATSYL)
modusBarbara = ["all M are P", "all S are M"] :> "all S are P"
