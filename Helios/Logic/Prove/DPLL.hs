{-

* David & Putnam (1960). "A Computing Procedure for Quantification Theory".
* David, Logemann & Loveland (1962). "A Machine Program for Theorem-Proving".

-}

module Helios.Logic.Prove.DPLL where

import           Control.Monad.State.Strict

import           Helios
import           Helios.Data.List ( (+:), intercalate )
import qualified Helios.Data.Map as Map

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

--------------------------------------------------------------------------------
-- Prenex normal form
--------------------------------------------------------------------------------

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

dropPrefix :: Formula q s r c v -> Formula q s r c v
dropPrefix (Quantifier _ _ f)
  = dropPrefix f
dropPrefix f
  = f

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

testFormula1
  :: Formula Quantifier Symbol Char () Char
testFormula1
  = Symbol IMPL
    [ Symbol OR
      [ Relation 'P' []
      , Quantifier EXISTS 'x' (Relation 'Q' [Var 'x'])
      ]
    , Quantifier FORALL 'z' (Relation 'R' [Var 'z'])
    ]

-- FIXME: q -> Empty in return type
matrix
  :: Formula q s r c v
  -> Formula q s r c v
matrix f
  | isPrenexForm f
    = dropPrefix f
  | otherwise
    = error "formula not in prenex form"

--------------------------------------------------------------------------------
-- Conjuntive normal form
--------------------------------------------------------------------------------

-- FIXME: Symbol -> Symbol' in return type
toNegationNormalForm
  :: Formula q Symbol r c v
  -> Formula q Symbol r c v
toNegationNormalForm
  = rewrite parallelLeftOuter toNegationNormalForm'
  where
    toNegationNormalForm'
      :: Formula q Symbol r c v
      -> Reduced (Formula q Symbol r c v)
    toNegationNormalForm' (Symbol IMPL [f1, f2])
      = Reduced $ Symbol OR [Symbol NOT [f1], f2]
    toNegationNormalForm' (Symbol NOT [Symbol OR [f1, f2]])
      = Reduced $ Symbol AND [Symbol NOT [f1], Symbol NOT [f2]]
    toNegationNormalForm' (Symbol NOT [Symbol AND [f1, f2]])
      = Reduced $ Symbol OR [Symbol NOT [f1], Symbol NOT [f2]]
    toNegationNormalForm' (Symbol NOT [Symbol NOT [f]])
      = Reduced $ f
    toNegationNormalForm' f
      = Irreducable f

toConjunctiveNormalForm
  :: Formula q Symbol r c v
  -> Formula q Symbol r c v
toConjunctiveNormalForm
  = rewrite parallelLeftOuter toConjunctiveNormalForm'
  . toNegationNormalForm
  where
    toConjunctiveNormalForm'
      :: Formula q Symbol r c v
      -> Reduced (Formula q Symbol r c v)
    toConjunctiveNormalForm' (Symbol OR [f1, Symbol AND [f2, f3]])
      = Reduced $ Symbol AND [Symbol OR [f1, f2], Symbol OR [f1, f3]]
    toConjunctiveNormalForm' f
      = Irreducable f

--------------------------------------------------------------------------------
-- Skolemization
--------------------------------------------------------------------------------

type Interp f v = Map.Map v (f v)

empty
  :: Interp f v
empty
  = Map.empty

-- FIXME: does not check does if already interpreted
-- FIXME: does not substitute in the existing interpretation
extend
  :: (Ord v)
  => v
  -> f v
  -> Interp f v
  -> Interp f v
extend x f i
  = Map.insert x f i

substTerm
  :: (Ord v)
  => Interp (Term c) v
  -> Term c v
  -> Term c v
substTerm i (Var x)
  = Map.findWithDefault (Var x) x i
substTerm i (Con c ts)
  = Con c (map (substTerm i) ts)

data Skolem s a = Skolem s | Normal a
  deriving (Eq, Show)

fresh
  :: Enum a => State a a
fresh
  = do
      x <- get
      put (succ x)
      return x

skolemize
  :: (Ord v)
  => Formula Quantifier s r c v
  -> Formula Quantifier s r (Skolem Int c) v
skolemize f
  = evalState (skolemize' [] empty f) 0
  where
    skolemize'
      :: (Ord v)
      => [v]
      -> Interp (Term (Skolem Int c)) v
      -> Formula Quantifier s r c v
      -> State Int (Formula Quantifier s r (Skolem Int c) v)
    skolemize' xs i (Quantifier FORALL x f)
      = Quantifier FORALL x <$> skolemize' (xs +: x) i f
    skolemize' xs i (Quantifier EXISTS x f)
      = do
          c <- fresh
          skolemize' xs (extend x (Con (Skolem c) (map Var xs)) i) f
    skolemize' xs i (Symbol s fs)
      = Symbol s <$> mapM (skolemize' xs i) fs
    skolemize' xs i (Relation r ts)
      = return $ Relation r (map (substTerm i) (map liftTerm ts))

    liftTerm
      :: Term c v
      -> Term (Skolem n c) v
    liftTerm (Var x)
      = Var x
    liftTerm (Con c ts)
      = Con (Normal c) (map liftTerm ts)

testFormula2
  = Quantifier EXISTS "x1"
  $ Quantifier FORALL "x2"
  $ Quantifier EXISTS "x3"
  $ Quantifier FORALL "x4"
  $ Relation "M" [Var "x1", Var "x2", Var "x3", Var "x4"]
