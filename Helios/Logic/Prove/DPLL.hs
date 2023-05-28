{-

* David & Putnam (1960). "A Computing Procedure for Quantification Theory".
* David, Logemann & Loveland (1962). "A Machine Program for Theorem-Proving".

-}

module Helios.Logic.Prove.DPLL where

import Helios.Data.List ( intercalate )

--------------------------------------------------------------------------------
-- Terms
--------------------------------------------------------------------------------

data Term c v
  = Var v
  | Con c [Term c v]
  deriving (Show)

printTerm :: (v -> String) -> (c -> String) -> Term c v -> String
printTerm printV _ (Var v)
  = printV v
printTerm printV printC (Con c ts)
  = printC c ++ "(" ++ intercalate "," (map (printTerm printV printC) ts) ++ ")"

--------------------------------------------------------------------------------
-- Formulas
--------------------------------------------------------------------------------

data Formula q s r c v
  = Quantifier q v (Formula q s r c v)
  | Symbol s [Formula q s r c v]
  | Relation r [Term c v]
  deriving (Show)

data Quantifier
  = FORALL
  | EXISTS
  deriving (Show)

invertQuantifier :: Quantifier -> Quantifier
invertQuantifier FORALL
  = EXISTS
invertQuantifier EXISTS
  = FORALL

data Symbol
  = TRUE
  | FALSE
  | NOT
  | AND
  | OR
  | IMPL
  deriving (Eq, Show)

data Relation
  = EQUALS
  deriving (Show)

isQuantifierFree :: Formula q s r c v -> Bool
isQuantifierFree (Quantifier _ _ _)
  = False
isQuantifierFree (Symbol _ fs)
  = all isQuantifierFree fs
isQuantifierFree (Relation _ _)
  = True

isPrenexForm :: Formula q s r c v -> Bool
isPrenexForm (Quantifier _ _ f)
  = isPrenexForm f
isPrenexForm f
  = isQuantifierFree f

data Reduced a = Reduced a | Irreducable a

instance Functor Reduced where
  fmap f (Reduced x)
    = Reduced (f x)
  fmap f (Irreducable x)
    = Irreducable (f x)

reduced :: (a -> b) -> (a -> b) -> Reduced a -> b
reduced f _ (Reduced x)
  = f x
reduced _ g (Irreducable x)
  = g x

isReduced :: Reduced a -> Bool
isReduced
  = reduced (const True) (const False)

many :: (a -> Reduced a) -> a -> a
many f x
  = reduced (many f) id (f x)

type ReductionStrategy q s r c v
  =  (Formula q s r c v -> Reduced (Formula q s r c v))
  -> Formula q s r c v
  -> Reduced (Formula q s r c v)

rewrite
  :: ReductionStrategy q s r c v
  -> (Formula q s r c v -> Reduced (Formula q s r c v))
  -> Formula q s r c v
  -> Formula q s r c v
rewrite reductionStrategy reductionRule x
  = many (reductionStrategy reductionRule) x

parallelLeftOuter
  :: ReductionStrategy q s r c v
parallelLeftOuter r f
  | Reduced f' <- r f
    = Reduced f'
parallelLeftOuter r (Quantifier q v f)
  = fmap (Quantifier q v) (parallelLeftOuter r f)
parallelLeftOuter r (Symbol s fs)
  | let fs' = map (parallelLeftOuter r) fs
  , any isReduced fs'
    = Reduced $ Symbol s (map (reduced id id) fs')
parallelLeftOuter r f
  = Irreducable f

-- FIXME: use proper rewrite system
-- FIXME: missing some additional inhabitation conditions
toPrenexForm
  :: Formula Quantifier Symbol r c v
  -> Formula Quantifier Symbol r c v
toPrenexForm
  = rewrite parallelLeftOuter toPrenexForm'
  where
    toPrenexForm'
      :: Formula Quantifier Symbol r c v
      -> Reduced (Formula Quantifier Symbol r c v)
    toPrenexForm' (Symbol NOT [Quantifier q x f])
      = Reduced $ Quantifier (invertQuantifier q) x (Symbol NOT [f])
    toPrenexForm' (Symbol s [Quantifier q x f1, f2])
      | s `elem` [AND, OR]
        = Reduced $ Quantifier q x (Symbol s [f1, f2])
    toPrenexForm' (Symbol s [f1, Quantifier q x f2])
      | s `elem` [AND, OR]
        = Reduced $ Quantifier q x (Symbol s [f1, f2])
    toPrenexForm' (Symbol IMPL [Quantifier q x f1, f2])
        = Reduced $ Quantifier (invertQuantifier q) x (Symbol IMPL [f1, f2])
    toPrenexForm' (Symbol IMPL [f1, Quantifier q x f2])
        = Reduced $ Quantifier q x (Symbol IMPL [f1, f2])
    toPrenexForm' f
        = Irreducable f

testFormula
  :: Formula Quantifier Symbol Char () Char
testFormula
  = Symbol IMPL
    [ Symbol OR
      [ Relation 'P' []
      , Quantifier EXISTS 'x' (Relation 'Q' [Var 'x'])
      ]
    , Quantifier FORALL 'z' (Relation 'R' [Var 'z'])
    ]

