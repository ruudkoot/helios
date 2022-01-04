module Dataset
( __FILE__
, __MODULE__
, datasetPath
) where

import Language.Haskell.TH.Syntax

import Helios.Data.List

__FILE__, __MODULE__ :: Q Exp
__FILE__    = lift . loc_filename    =<< location
__MODULE__  = lift . loc_module      =<< location

datasetPath :: String -> String -> FilePath
datasetPath s1 s2
  = dropSuffix ".hs" s1 ++ "/" ++ s2
