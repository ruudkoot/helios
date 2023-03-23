{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Helios.Data.Graph
( Graph
, empty
, fromTriples
, undirectedFromTriples
, toTriples
, edge
, outgoing
) where

import qualified Helios.Data.List as List
import qualified Helios.Data.Map as Map

newtype Graph v e
  = Graph { edges :: Map.Map v (Map.Map v e) }
  deriving (Show)

--------------------------------------------------------------------------------
-- * Construct
--------------------------------------------------------------------------------

empty :: Graph v e
empty = Graph { edges = Map.empty }

fromTriples :: forall v e. Ord v => [(v,e,v)] -> Graph v e
fromTriples triples
  = Graph { edges = List.foldl' insertTriple Map.empty triples }
  where
    insertTriple
      :: Map.Map v (Map.Map v e)
      -> (v,e,v)
      -> Map.Map v (Map.Map v e)
    insertTriple edges (v1,e,v2)
      = Map.insertWith Map.union v1 (Map.singleton v2 e) edges

undirectedFromTriples :: Ord v => [(v,e,v)] -> Graph v e
undirectedFromTriples triples
  = fromTriples (triples ++ map (\(v1,e,v2) -> (v2,e,v1)) triples)

toTriples :: Graph v e -> [(v,e,v)]
toTriples Graph{..}
  = concatMap
      (\(v1,m) -> map (\(v2,e) -> (v1,e,v2)) (Map.toList m))
      (Map.toList edges)

--------------------------------------------------------------------------------
-- * Query
--------------------------------------------------------------------------------

edge :: Ord v => Graph v e -> v -> v -> e
edge g v1 v2
  = edges g Map.! v1 Map.! v2

outgoing :: Ord v => Graph v e -> v -> [(v,e)]
outgoing g v
  = Map.toList (edges g Map.! v)