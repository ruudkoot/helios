{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module Helios.Data.Dynamic.Types
  ( module Data.Proxy
  , module Data.Typeable
  , Dynamic
  , Packet(..)
  ) where

import Data.Proxy
import Data.Typeable

type Dynamic a = (Typeable a, Eq a, Ord a, Show a)

data Packet = forall a. Dynamic a => Packet a

instance Show Packet where
  show (Packet x) = "«" ++ show x ++ ":" ++ show (typeOf x) ++ "»"
