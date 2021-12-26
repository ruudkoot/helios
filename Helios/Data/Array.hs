module Helios.Data.Array
( module Data.Array
, sum
) where

import qualified Prelude
import           Prelude
  hiding ( sum )

import           Data.Array

sum = Prelude.sum . elems
