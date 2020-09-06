{-# LANGUAGE RankNTypes #-}

module Helios.Data.Relation
  ( Relation
  ) where

import Data.Typeable

data Relation =
  Relation
  { cols :: Column
  }
  deriving (Show)

data Column =
  Column
  { colName :: String
  , colType :: TypeRep
  , colData :: forall a. (Typeable a, Show a) => a
  }

instance Show Column where
  show (Column n t x) =
    n ++ ":" ++ show t ++ ":" ++ show x

mkRelation :: Typeable a => String -> a -> Relation
mkRelation s x =
  Relation
  { cols =
      Column
      { colName = s
      , colType = typeOf x
      , colData = undefined
      }
  }