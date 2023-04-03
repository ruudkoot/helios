{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Helios.Search.Graph.Example.RouteFinding
( City(..)
, roadmap
, main
) where

import qualified Helios.Data.Array  as Array
import qualified Helios.Data.Graph  as Graph
import qualified Helios.Data.List   as List
import qualified Helios.Data.Maybe  as Maybe
import           Helios.Data.Queue
import qualified Helios.Data.Set    as Set
import           Helios.Data.Tuple

import           Helios.Search.Graph

data City
  = Oradea | Zerind | Arad | Timisoara | Lugoj | Mehadia | Dobreta | Sibiu
  | Rimnicu_Vilcea | Craiova | Fagaras | Pitesti | Bucharest | Giurgiu | Neamt
  | Iasi | Vaslui | Urziceni | Hirsova | Eforie
  deriving (Bounded, Enum, Eq, Ord, Show)

roadmap :: Graph.Graph City Int
roadmap
  = Graph.undirectedFromTriples
    [(Oradea,71,Zerind)
    ,(Zerind,75,Arad)
    ,(Arad,118,Timisoara)
    ,(Timisoara,111,Lugoj)
    ,(Lugoj,70,Mehadia)
    ,(Mehadia,75,Dobreta)
    ,(Dobreta,120,Craiova)
    ,(Oradea,151,Sibiu)
    ,(Arad,140,Sibiu)
    ,(Sibiu,99,Fagaras)
    ,(Sibiu,80,Rimnicu_Vilcea)
    ,(Rimnicu_Vilcea,146,Craiova)
    ,(Rimnicu_Vilcea,97,Pitesti)
    ,(Pitesti,138,Craiova)
    ,(Fagaras,211,Bucharest)
    ,(Pitesti,101,Bucharest)
    ,(Bucharest,90,Giurgiu)
    ,(Bucharest,85,Urziceni)
    ,(Urziceni,98,Hirsova)
    ,(Hirsova,86,Eforie)
    ,(Urziceni,142,Vaslui)
    ,(Vaslui,92,Iasi)
    ,(Iasi,87,Neamt)
    ]

roadmap' :: Graph.Graph City Int
roadmap'
  = Graph.undirectedFromTriples
    [(Oradea,71,Zerind)
    ,(Zerind,75,Arad)
    --,(Arad,118,Timisoara)
    ,(Timisoara,111,Lugoj)
    ,(Lugoj,70,Mehadia)
    ,(Mehadia,75,Dobreta)
    ,(Dobreta,120,Craiova)
    ,(Oradea,151,Sibiu)
    ,(Arad,140,Sibiu)
    ,(Sibiu,99,Fagaras)
    ,(Sibiu,80,Rimnicu_Vilcea)
    ,(Rimnicu_Vilcea,146,Craiova)
    ,(Rimnicu_Vilcea,97,Pitesti)
    ,(Pitesti,138,Craiova)
    ,(Fagaras,211,Bucharest)
    ,(Pitesti,101,Bucharest)
    ,(Bucharest,90,Giurgiu)
    --,(Bucharest,85,Urziceni)
    ,(Urziceni,98,Hirsova)
    ,(Hirsova,86,Eforie)
    ,(Urziceni,142,Vaslui)
    ,(Vaslui,92,Iasi)
    ,(Iasi,87,Neamt)
    ]

routingProblem :: City -> City -> Problem City City
routingProblem start end
  = Problem
    { initialState
        = start
    , successorFn
        = \city -> map (dup . fst) (Graph.outgoing roadmap city)
    , goalTest
        = \city -> city == end
    , stepCost
        = \from _to to -> Graph.edge roadmap from to
    }

solveRouting :: City -> City -> [City]
solveRouting start end
  = start : reverse (map state (unPath (head paths)))
  where
    paths
      = breadthFirstSearch (routingProblem start end)

touringProblem
  :: (Ord node)
  => Graph.Graph node Int
  -> node
  -> Set.Set node
  -> Problem node (node, Set.Set node)
touringProblem graph start toVisit
  = Problem
    { initialState
        = (start, Set.singleton start)
    , successorFn
        = \(city, visited) ->
            map (\(city', _) -> (city', (city', city' `Set.insert` visited)))
                (Graph.outgoing graph city)
    , goalTest
        = \(city, visited) ->
            city == start && visited == toVisit
    , stepCost
        = \(from, _) _to (to, _) ->
            Graph.edge graph from to
    }

solveTouring
  :: (Ord node)
  => Graph.Graph node Int
  -> node
  -> Set.Set node
  -> [node]
solveTouring graph start toVisit
  = start : reverse (map (fst . state) (unPath (head paths)))
  where
    paths
      = breadthFirstSearch (touringProblem graph start toVisit)

main :: IO ()
main = do
  print $ solveTouring roadmap' Bucharest (Set.fromList [Bucharest, Fagaras, Sibiu, Rimnicu_Vilcea, Pitesti, Craiova, Giurgiu, Oradea, Zerind, Arad])
