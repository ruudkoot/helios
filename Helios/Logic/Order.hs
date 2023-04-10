{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- https://en.wikipedia.org/wiki/Binary_relation

module Helios.Logic.Order where

import qualified Helios.Data.DisjointSets as DS
import qualified Data.Set as S

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

data Rel a =
  Rel
  { carrier :: [a]
  , rel :: S.Set (a,a)
  } deriving ( Eq, Show )

-- TODO: this generates isomorphic relations
rels :: (Bounded a, Enum a, Ord a) => [Rel a]
rels =
  [ Rel carrier (S.fromList rel)
  | upperBound <- [minBound .. ]
  , let carrier = init [minBound .. upperBound]
  , rel <- powerset [ (x,y) | x <- carrier, y <- carrier ]
  ]

closeRefl :: (Ord a) => Rel a -> Rel a
closeRefl (Rel carrier rel) =
  Rel
    carrier
    (S.union rel (S.fromList [ (x,x) | x <- carrier ]))

closeSymm :: (Ord a) => Rel a -> Rel a
closeSymm (Rel carrier rel) =
  Rel
    carrier
    (S.union rel (S.fromList [ (y,x) | (x,y) <- S.toList rel ]))

fixedPoint :: (Eq a) => (a -> a) -> a -> a
fixedPoint f x = let y = f x in if x == y then x else fixedPoint f y

closeTrans :: (Ord a) => Rel a -> Rel a
closeTrans = fixedPoint step
  where step (Rel carrier rel) =
          Rel
            carrier
            (S.union rel
              (S.fromList
                [ (x,z)
                | (x,y) <- S.toList rel
                , (y',z) <- S.toList rel
                , y == y'
                ]
              )
            )

-- FIXME: O(n^2)
isTrans :: (Ord a) => Rel a -> Bool
isTrans (Rel carrier rel) =
  and
    [ S.member (x,z) rel
    | (x,y) <- S.toList rel
    , (y',z) <- S.toList rel
    , y == y'
    ]

isRefl :: (Ord a) => Rel a -> Bool
isRefl (Rel carrier rel) =
  all (\x -> S.member (x,x) rel) carrier

isIrrefl :: (Ord a) => Rel a -> Bool
isIrrefl (Rel carrier rel) =
  all (\x -> not (S.member (x,x) rel)) carrier

isSymm :: (Ord a) => Rel a -> Bool
isSymm (Rel carrier rel) =
  all (\(x,y) -> S.member (y,x) rel) rel

isAntiSymm :: (Ord a) => Rel a -> Bool
isAntiSymm (Rel carrier rel) =
  and
    [ x == y || not (S.member (y,x) rel)
    | (x,y) <- S.toList rel
    ]

isEquiv :: (Ord a) => Rel a -> Bool
isEquiv rel =
  isRefl rel && isTrans rel && isSymm rel

isPartialOrder :: (Ord a) => Rel a -> Bool
isPartialOrder rel =
  isRefl rel && isTrans rel && isAntiSymm rel

isStrictOrder :: (Ord a) => Rel a -> Bool
isStrictOrder rel =
  isIrrefl rel && isTrans rel

isQuasiOrder :: (Ord a) => Rel a -> Bool
isQuasiOrder rel =
  isRefl rel && isTrans rel

isLinearOrder :: (Ord a) => Rel a -> Bool
isLinearOrder (Rel carrier rel) =
  and
    [ x == y || S.member (x,y) rel || S.member (y,x) rel
    | x <- carrier
    , y <- carrier
    ]

quotients :: (Ord a) => Rel a -> [S.Set a]
quotients r
  | isEquiv r
      = map S.fromList
      $ DS.toLists
      $ foldr
          (\(x,y) ds -> DS.union ds x y)
          (DS.makeSets (carrier r))
          (rel r)

--------------------------------------------------------------------------------
-- Posets
--------------------------------------------------------------------------------

newtype Poset a = Poset (Rel a)

{-
-- ∀n∈M.n≥m=>n=m
isMaximal :: Poset a -> S.Set a -> a -> Bool
isMaximal (Poset poset) m n =
  all
    (\n -> n == m)
    [ n
    | (n',m') <- S.toList (rel poset)
    , m'' <- m
    , n' == n
    , m' == m''
    ]
-}