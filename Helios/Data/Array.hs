module Helios.Data.Array
( module Data.Array
, select
, sum
) where

import qualified Prelude
import           Prelude
  hiding ( sum )

import           Data.Array -- FIXME: Data.Array.IArray

-- | Used by Relation.Dynamic.filter - otherwise useless?
select :: Array Int Bool -> Array Int e -> Array Int e
select bitmap arr
  = listArray (0, length xs - 1) xs
  where
    xs = map snd (filter fst (zip (elems bitmap) (elems arr)))

sum = Prelude.sum . elems
