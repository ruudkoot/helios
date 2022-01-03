{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}

-- delayed operations in Relation data type
-- deep vs shallow expressions passed to combinators
-- type-specialized Columns and Indices

module Helios.Data.Relation.Dynamic
( Attribute
, Relation
, addIndex
, relationR
, attributes
, arity
, cardinality
, extend
, filter
, project
, Aggregator(..)
, aggregate
) where

import           Prelude hiding ( filter )

import           Control.Arrow ((***))

import qualified Helios.Data.Array as Array
import           Helios.Data.Dynamic
import qualified Helios.Data.List as List
import qualified Helios.Data.Map as Map
import           Helios.Data.Maybe
import qualified Helios.Data.Set as Set
import qualified Helios.Data.String as String

--------------------------------------------------------------------------------
-- * Attributes
--------------------------------------------------------------------------------

newtype Attribute
  = Attribute { attribute :: String }
  deriving (Eq, Ord)

instance String.IsString Attribute where
  fromString = Attribute

instance Show Attribute where
  show = attribute

--------------------------------------------------------------------------------
-- * Columns
--------------------------------------------------------------------------------

data ColumnList
  = forall a. Dynamic a => ColumnList { columnData' :: [a] }

-- FIXME: make work on empty rows
toColumnList :: [Packet] -> ColumnList
toColumnList [Packet x]
  = ColumnList [x]
toColumnList (Packet x : ps)
  = case toColumnList ps of
      ColumnList xs -> ColumnList (fromJust (cast x) : xs)

data Column
  = forall a. Dynamic a => Column { columnData :: Array.Array Int a }

fromColumnList :: ColumnList -> Column
fromColumnList (ColumnList x)
  = Column (Array.listArray (0, length x - 1) x)

toColumnList' :: Column -> ColumnList
toColumnList' (Column x)
  = ColumnList (Array.elems x)

fromColumnList' :: ColumnList -> [Packet]
fromColumnList' (ColumnList x)
  = map Packet x

toColumn :: [Packet] -> Column
toColumn
  = fromColumnList . toColumnList

fromColumn :: Column -> [Packet]
fromColumn
  = fromColumnList' . toColumnList'

-- FIXME: make work on empty columns
typeOfElem :: Typeable a => Array.Array Int a -> TypeRep
typeOfElem xs
  = typeOf (xs Array.! 0)

columnType :: Column -> TypeRep
columnType (Column x)
  = typeOfElem x

columnShown :: Column -> [String]
columnShown (Column xs)
  = map show (Array.elems xs)

cardinality' :: Map.Map Attribute Column -> Int
cardinality'
  = cardinality'' . snd . Map.findMin

cardinality'' :: Column -> Int
cardinality'' (Column x)
  = (succ . snd) (Array.bounds x)

packetize
  :: [Attribute]
  -> Relation
  -> [[Packet]]
packetize as r
  = foldr f e as
  where
    e
      = replicate (cardinality r) []
    f a x
      = zipWith (:) (fromColumn (getColumn a r)) x

unpacketize
  :: [Attribute]
  -> [[Packet]]
  -> Relation
unpacketize as pss
  = relationFromColumns as
  $ map toColumn
  $ List.transpose pss

--------------------------------------------------------------------------------
-- * Indices
--------------------------------------------------------------------------------

data Index
  = forall a. Dynamic a => IndexNext (Map.Map a Index)
  | forall a. Dynamic a => IndexLast (Map.Map a Int)

deriving instance Show Index

addIndex :: [Attribute] -> Relation -> Relation
addIndex as rel
  = rel { indices = Map.insert as (createIndex as (columns rel)) (indices rel) }

createIndex :: [Attribute] -> Map.Map Attribute Column -> Index
createIndex as cs
  = List.foldl'
      (createIndex' cs as)
      (emptyIndex cs as)
      [0 .. cardinality' cs - 1]

createIndex'
  :: Map.Map Attribute Column
  -> [Attribute]
  -> Index
  -> Int
  -> Index
createIndex' cs [a] (IndexLast m) r
  = case cs Map.! a of
      Column c ->
        let v = (fromJust . cast) (c Array.! r) -- FIXME: Use Typeable.:~:?
        in IndexLast (Map.insertWith errorDuplicate v r m)
createIndex' cs (a:as) (IndexNext m) r
  = case cs Map.! a of
      Column c ->
        let v = (fromJust . cast) (c Array.! r)
            f Nothing = Just (createIndex' cs as (emptyIndex cs as) r)
            f (Just m) = Just (createIndex' cs as m r)
        in IndexNext (Map.alter f v m)

emptyIndex :: Map.Map Attribute Column -> [Attribute] -> Index
emptyIndex cs [a]
  = case (cs Map.! a) of
      Column (c :: Array.Array Int a) ->
        IndexLast (Map.empty :: Map.Map a Int)
emptyIndex cs (a:as)
  = case (cs Map.! a) of
      Column (c :: Array.Array Int a) ->
        IndexNext (Map.empty :: Map.Map a Index)

--------------------------------------------------------------------------------
-- * Relations
--------------------------------------------------------------------------------

data Relation
  = Relation
    { columns :: Map.Map Attribute Column
    , indices :: Map.Map [Attribute] Index
    }

instance Show Relation where
  show = showRelation

-- FIXME: add primary key
relationFromColumns :: [Attribute] -> [Column] -> Relation
relationFromColumns as cs
  = Relation
    { columns
        = Map.fromList (zip as cs)
    , indices
        = Map.empty
    }

getColumn :: Attribute -> Relation -> Column
getColumn a r
  = columns r Map.! a

applyRelation f g rel
  = Relation
    { columns
        = f (columns rel)
    , indices
        = g (indices rel)
    }

--------------------------------------------------------------------------------
-- * Tables
--------------------------------------------------------------------------------

data Table
  = forall r. Row r => Table { rows :: [r] }

--instance Show Table where
--  show = showTable

--------------------------------------------------------------------------------
-- * Constructors
--------------------------------------------------------------------------------

-- | Create a relation from a list of rows.
relationR :: forall r. (Row r) => [Attribute] -> [r] -> Relation
relationR header rows
  | widthH /= widthR
    = errorWidthMismatch widthH widthR
  | otherwise
    = Relation (Map.fromList (zip header cols)) (Map.empty)
  where widthH = length header
        widthR = rowWidth (Proxy :: Proxy r)
        cols = map mkCol [0 .. widthR - 1]
        mkCol :: Int -> Column
        mkCol n = toColumn (map (rowIndex n) rows)

--------------------------------------------------------------------------------
-- * Destructors
--------------------------------------------------------------------------------

toList :: Relation -> [(Attribute, Column)]
toList = Map.toList . columns

attributes :: Relation -> [(Attribute, TypeRep)]
attributes = map (id *** columnType) . toList

arity :: Relation -> Int
arity = length . Map.keys . columns

cardinality :: Relation -> Int
cardinality = cardinality' . columns

--------------------------------------------------------------------------------
-- * Show
--------------------------------------------------------------------------------

showRelation :: Relation -> String
showRelation rel
  = List.intercalate "\n" rows3
  where
    cols1 :: [(Attribute,Column)]
      = toList rel
    cols2 :: [[String]]
      = map (\(h,c) -> (show h ++ ":" ++ show (columnType c)) : columnShown c) cols1
    widths :: [Int]
      = map (maximum . map length) cols2
    lineT
      = mkLine "┌" "┬" "┐" widths
    lineM
      = mkLine "├" "┼" "┤" widths
    lineB
      = mkLine "└" "┴" "┘" widths
    cols3 :: [[String]]
      = zipWith (\n -> map (String.padLeft ' ' n)) widths cols2
    rows1
      = List.transpose cols3
    (h2:rows2)
      = map (String.bracket1 "│" . List.intercalate "│") rows1
    rows3
      = [lineT,h2,lineM] ++ rows2 ++ [lineB]

mkLine :: String -> String -> String -> [Int] -> String
mkLine l m r
  = String.bracket2 l r . List.intercalate m . map (flip replicate '─')

--------------------------------------------------------------------------------
-- * Algebra
--------------------------------------------------------------------------------

-- FIXME: multiple output columns
extend
  :: forall r a. (Dynamic r, DynamicFunction r a)
  => [Attribute]
  -> Attribute
  -> a
  -> Relation
  -> Relation
extend as a f r
  = applyRelation extendC id r
  where
    extendC columns
      = Map.insertWithKey errorExtendNotDisjoint a newColumn columns
    newColumn
      = Column
      $ Array.listArray (0, cardinality r - 1)
      $ fmap (applyDynamicFunction f :: [Packet] -> r) (packetize as r)

-- FIXME: express in terms of 'extend'
filter
  :: DynamicFunction Bool a
  => [Attribute]
  -> a
  -> Relation
  -> Relation
filter as p r
  = applyRelation filterC filterI r
  where
    filterC columns
      = Map.map filterC' columns
      where
        bitmap
          = Array.listArray (0, cardinality r - 1)
          $ fmap (applyDynamicFunction p) (packetize as r)
        filterC' (Column c)
          = Column (Array.select bitmap c)
    filterI
      -- FIXME: implement
      = id

project
  :: [Attribute]
  -> Relation
  -> Relation
project as rel
  = Relation
    { columns
        = Map.restrictKeys (columns rel) (Set.fromList as)
    , indices
        = Map.restrictKeys
            (indices rel)
            (Set.filter
              (\ks -> Set.fromList ks `Set.isSubsetOf` Set.fromList as)
              (Set.fromList (Map.keys (indices rel)))
            )
    }

-- TODO: distinct
data Aggregator
  = COUNT | MIN | MAX | SUM | AVG

-- TODO: aggregateWith
-- TODO: mapReduce / foldMap
aggregate
  :: [Attribute]
  -> [(Attribute, Aggregator)]
  -> Relation
  -> Relation
aggregate as gs r
  = toRel $ fmap post $ foldr f Map.empty (zip as' gs')
  where
    toRel :: Map.Map [Packet] [Packet] -> Relation
    toRel
      = unpacketize (as ++ map fst gs)
      . map (\(x,y) -> x ++ y)
      . Map.toList
    as'
      = packetize as r
    gs'
      = fmap (fmap (,1)) (packetize (map fst gs) r)
    f :: ([Packet], [(Packet, Int)])
      -> Map.Map [Packet] [(Packet, Int)]
      -> Map.Map [Packet] [(Packet, Int)]
    f (a,g) m
      = Map.insertWith i a g m
    i :: [(Packet, Int)] -> [(Packet, Int)] -> [(Packet, Int)]
    i old new
      = zipWith3 j (map snd gs) old new
    j :: Aggregator -> (Packet, Int) -> (Packet, Int) -> (Packet, Int)
    j COUNT (p, n) _
      = (p, n + 1)
    j MIN (Packet p, n) (Packet q, _)
      | Int <- typecase p
      , Just p' <- cast p
      , Just q' <- cast q
        = (Packet (p' `min` q' :: Int), n + 1)
      | Integer <- typecase p
      , Just p' <- cast p
      , Just q' <- cast q
        = (Packet (p' `min` q' :: Integer), n + 1)
      | Double <- typecase p
      , Just p' <- cast p
      , Just q' <- cast q
        = (Packet (p' `min` q' :: Double), n + 1)
    j MAX (Packet p, n) (Packet q, _)
      | Int <- typecase p
      , Just p' <- cast p
      , Just q' <- cast q
        = (Packet (p' `max` q' :: Int), n + 1)
      | Integer <- typecase p
      , Just p' <- cast p
      , Just q' <- cast q
        = (Packet (p' `max` q' :: Integer), n + 1)
      | Double <- typecase p
      , Just p' <- cast p
      , Just q' <- cast q
        = (Packet (p' `max` q' :: Double), n + 1)
    j SUM (Packet p, n) (Packet q, _)
      | Int <- typecase p
      , Just p' <- cast p
      , Just q' <- cast q
        = (Packet (p' + q' :: Int), n + 1)
      | Integer <- typecase p
      , Just p' <- cast p
      , Just q' <- cast q
        = (Packet (p' + q' :: Integer), n + 1)
      | Double <- typecase p
      , Just p' <- cast p
      , Just q' <- cast q
        = (Packet (p' + q' :: Double), n + 1)
    j AVG (Packet p, n) (Packet q, _)
      | Double <- typecase p
      , Just p' <- cast p
      , Just q' <- cast q
        = (Packet (p' + q' :: Double), n + 1)
    post :: [(Packet, Int)] -> [Packet]
    post ps
      = zipWith post' (map snd gs) ps
      where
        post' :: Aggregator -> (Packet, Int) -> Packet
        post' COUNT (_, n)
          = Packet n
        post' AVG (Packet p, n)
          | Double <- typecase p
          , Just (p' :: Double) <- cast p
            = Packet (p' / fromIntegral n)
        post' _ (p, _)
          = p

--------------------------------------------------------------------------------
-- * Errors
--------------------------------------------------------------------------------

errorDuplicate
  = error "Duplicate key"

errorExtendNotDisjoint k _ _
  = error $ "extend: not disjoint (" ++ show k ++ ")"

errorFilterCast
  = error "filter: cast"

errorWidthMismatch widthH widthR
  = error $
      "Width of header (" ++ show widthH ++ ") and rows (" ++ show widthR ++
      ") do not match."
