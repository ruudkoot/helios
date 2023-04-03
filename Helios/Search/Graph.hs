{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

--------------------------------------------------------------------------------
--
-- Graph search algorithms (BFS, DFS, A*, ...)
--
-- * Russell & Norvig. Artificial Intelligence: A Modern Approach, 2nd Edition.
--
--------------------------------------------------------------------------------

module Helios.Search.Graph
( Problem(..)
, Node(..)
, Path(..)
, treeSearch
, makeNode, expand -- FIXME
, breadthFirstSearch
, depthFirstSearch
) where

import Helios.Data.Queue

--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

data Problem action state
  = Problem
    { initialState :: state
    , successorFn :: state -> [(action, state)] -- alternatively, operators
    , goalTest :: state -> Bool
    , stepCost :: state -> action -> state -> Int -- FIXME: can be real
    }

data Node action state
  = Node
    { state :: state
    , parentNode :: Maybe (Node action state) -- FIXME: no Maybe
    , action :: Maybe action -- FIXME: no Maybe
    , pathCost :: !Int -- FIXME: Double
    , depth :: !Int
    }
  deriving (Show)

makeNode :: state -> Node action state
makeNode state
  = Node
    { state = state
    , parentNode = Nothing
    , action = Nothing
    , pathCost = 0
    , depth = 0
    }

newtype Path action state
  = Path { unPath :: [Node action state] }
  deriving (Show)

solution :: Node action state -> Path action state
solution node
  = case parentNode node of
      Nothing ->
        Path []
      Just node' ->
        case solution node' of
          Path ns ->
            Path (node : ns)

--------------------------------------------------------------------------------
-- * Tree search
--------------------------------------------------------------------------------

treeSearch
  :: forall queue action state
  .  Queue queue (Node action state)
  => queue (Node action state)
  -> Problem action state
  -> [Path action state]
treeSearch fringe problem
  | isEmpty fringe
    = treeSearch' (insert (makeNode (initialState problem)) fringe)
  | otherwise
    = error "treeSearch: fringe not empty"
  where
    treeSearch' :: queue (Node action state) -> [Path action state]
    treeSearch' fringe
      | isEmpty fringe
        = []
      | goalTest problem (state node)
        = solution node : solutions
      | otherwise
        = solutions
      where
        (node, fringe')
          = removeFirst fringe
        solutions
          = treeSearch' (insertAll (expand problem node) fringe')

expand
  :: forall action state
  .  Problem action state
  -> Node action state
  -> [Node action state]
expand problem node
  = map toNode (successorFn problem (state node))
  where
    toNode :: (action, state) -> Node action state
    toNode (action, result)
      = Node
        { state = result
        , parentNode = Just node
        , action = Just action
        , pathCost = pathCost node + stepCost problem (state node) action result
        , depth = depth node + 1
        }

breadthFirstSearch
  :: forall action state
  .  Problem action state
  -> [Path action state]
breadthFirstSearch
  = treeSearch (empty :: FIFO (Node action state))

depthFirstSearch
  :: forall action state
  .  Problem action state
  -> [Path action state]
depthFirstSearch
  = treeSearch (empty :: LIFO (Node action state))
