{-# OPTIONS_GHC -Wall #-}

module Helios.Logic.Data
  ( disjoint
  ) where

import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------
-- Data.Map
--------------------------------------------------------------------------------

disjoint :: (Ord k) => Map.Map k v -> Map.Map k v -> Bool
xs `disjoint` ys = Map.null (xs `Map.intersection` ys)
