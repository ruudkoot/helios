{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Helios.Text.CSV
( Options(..)
, readFile
) where

import           Prelude hiding ( readFile )
import qualified System.IO

import Helios.Data.List
import Helios.Data.Relation.Dynamic

data Options
  = Options
    { header :: [Attribute]
    }

readFile :: Options -> FilePath -> IO Relation
readFile Options{..} fp = do
  s <- System.IO.readFile fp
  let tbl :: [[String]] = map (split (==',')) (filter (not . null) (lines s))
  let rel = relationR header tbl
  return rel
