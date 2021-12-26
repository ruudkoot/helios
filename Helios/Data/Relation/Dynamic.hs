{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

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
  , project
  ) where

import           Control.Arrow ((***))
import qualified Data.Array as Array
import qualified Data.List as List

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
fromColumnList (ColumnList x) = Column (Array.listArray (0, length x - 1) x)

toColumn :: [Packet] -> Column
toColumn
  = fromColumnList . toColumnList

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
  = List.foldl' (createIndex' cs as) (emptyIndex cs as) [0 .. cardinality' cs - 1]

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
  = let cols1 :: [(Attribute,Column)] = toList rel
        cols2 :: [[String]] = map (\(h,c) -> (show h ++ ":" ++ show (columnType c)) : columnShown c) cols1
        widths :: [Int] = map (maximum . map length) cols2
        lineT = mkLine "┌" "┬" "┐" widths
        lineM = mkLine "├" "┼" "┤" widths
        lineB = mkLine "└" "┴" "┘" widths
        cols3 :: [[String]] = zipWith (\n -> map (String.padLeft ' ' n)) widths cols2
        rows1 = List.transpose cols3
        (h2:rows2) = map (String.bracket1 "│" . List.intercalate "│") rows1
        rows3 = [lineT,h2,lineM] ++ rows2 ++ [lineB]
    in List.intercalate "\n" rows3 ++ show (indices rel)

mkLine :: String -> String -> String -> [Int] -> String
mkLine l m r
  = String.bracket2 l r . List.intercalate m . map (flip replicate '─')

--------------------------------------------------------------------------------
-- * Algebra
--------------------------------------------------------------------------------

project :: [Attribute] -> Relation -> Relation
project as rel =
  Relation
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

--------------------------------------------------------------------------------
-- * Errors
--------------------------------------------------------------------------------

errorDuplicate
  = error "Duplicate key"

errorWidthMismatch widthH widthR
  = error $
      "Width of header (" ++ show widthH ++ ") and rows (" ++ show widthR ++
      ") do not match."
