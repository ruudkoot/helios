{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Helios.Logic.Rule
  ( Rule(..)
  , Deriv
  , conclusion
  , axiom
  , forwardChain
  ) where

import Helios.Logic.Doc
import Helios.Logic.Term
import Helios.Logic.Infinite

--------------------------------------------------------------------------------
-- Inference rules and derivation trees
--------------------------------------------------------------------------------

data Rule term = [term] :> term
  deriving ( Eq, Ord, Show )

instance Functor Rule where
  fmap f (ts :> t) = fmap f ts :> f t

data Deriv term = [Deriv term] :>> term
  deriving ( Eq, Ord )

conclusion :: Deriv term -> term
conclusion (_ :>> t) = t

axiom :: term -> Deriv term
axiom t = [] :>> t

instance Functor Deriv where
  fmap f (ts :>> t) = fmap (fmap f) ts :>> f t

instance Show term => Show (Deriv term) where
  show = render . renderDeriv

renderDeriv :: (Show term) => Deriv term -> Doc
renderDeriv ([]:>>t) =
  emptyDoc <-> doc (show t)
renderDeriv ((d:ds):>>t) =
 foldr (\d' r -> renderDeriv d' <.> r) (renderDeriv d) ds <-> doc (show t)

--------------------------------------------------------------------------------
-- Forward chaining (Rete algorithm?)
--------------------------------------------------------------------------------

-- TODO: derivation trees
-- TODO: termination condition (no new theorems generated)
-- TODO: use Trie as theorem store
-- FIXME: does not work with axiom schemes
--        (do not mix finite set of theorems and infinite set of axioms?)
forwardChain :: [Rule (Term sig)] -> [Deriv (Term sig)] -> [Deriv (Term sig)]
forwardChain rules theorems =
  let theorems' = diagonal (theorems : map (applyRule theorems) rules)
  in theorems' ++ forwardChain rules theorems'

applyRule :: [Deriv (Term sig)] -> Rule (Term sig) -> [Deriv (Term sig)]
applyRule theorems (as' :> c') = applyRule' as' [] c' theorems
  where applyRule' [] ds c _ =
          [ds:>>c]
        applyRule' (_:_) _ _ [] =
          []
        applyRule' (a:as) ds c (d@(_:>>t):ts) =
          case match a t of
            Nothing ->
              applyRule' (a:as) ds c ts
            Just u ->
              applyRule' (map (apply u) as) (d:ds) (apply u c) theorems
                ++
              applyRule' (a:as) ds c ts