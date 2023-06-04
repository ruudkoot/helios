{-# LANGUAGE ScopedTypeVariables #-}

{-

* David & Putnam (1960). "A Computing Procedure for Quantification Theory".
* David, Logemann & Loveland (1962). "A Machine Program for Theorem-Proving".

-}

module Helios.Logic.Prove.DPLL where

import           Control.Monad.State.Strict

import           Helios
import           Helios.Data.List ( (+:), intercalate, sortNub, timesN )
import qualified Helios.Data.Map as Map

--------------------------------------------------------------------------------
-- Terms
--------------------------------------------------------------------------------

data Term c v
  = Var v
  | Con c [Term c v]
  deriving (Show)

mapConT
  :: (c -> c')
  -> Term c v
  -> Term c' v
mapConT f (Var x)
  = Var x
mapConT f (Con c ts)
  = Con (f c) (map (mapConT f) ts)

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

mapConF
  :: (c -> c')
  -> Formula q s r c v
  -> Formula q s r c' v
mapConF g (Quantifier q x f)
  = Quantifier q x (mapConF g f)
mapConF g (Symbol s fs)
  = Symbol s (map (mapConF g) fs)
mapConF g (Relation r ts)
  = Relation r (map (mapConT g) ts)

vars :: (Ord v) => Formula q s r c v -> [v]
vars f
  = sortNub (varsF f)
  where
    varsF (Quantifier _ _ f)
      = varsF f
    varsF (Symbol _ fs)
      = concatMap varsF fs
    varsF (Relation _ ts)
      = concatMap varsT ts

    varsT (Var x)
      = [x]
    varsT (Con _ ts)
      = concatMap varsT ts

cons :: (Ord c) => Formula q s r c v -> [(c, Int)]
cons f
  = sortNub (consF f)
  where
    consF (Quantifier _ _ f)
      = consF f
    consF (Symbol _ fs)
      = concatMap consF fs
    consF (Relation _ ts)
      = concatMap consT ts

    consT (Var _)
      = []
    consT (Con c ts)
      = (c, length ts) : concatMap consT ts

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

substFormula
  :: (Ord v)
  => Interp (Term c) v
  -> Formula q s r c v
  -> Formula q s r c v
substFormula i (Quantifier q x f)
  = Quantifier q x (substFormula i f)
substFormula i (Symbol s fs)
  = Symbol s (map (substFormula i) fs)
substFormula i (Relation r ts)
  = Relation r (map (substTerm i) ts)

data Skolem s a = Skolem s | Normal a
  deriving (Eq, Ord, Show)

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
  = Quantifier FORALL "x1"
  $ Quantifier EXISTS "x2"
  $ Quantifier EXISTS "x3"
  $ Quantifier FORALL "x4"
  $ Quantifier EXISTS "x5"
  $ Relation "R" [Var "x1", Var "x2", Var "x3", Var "x4", Var "x5"]

testFormula3
  = Quantifier EXISTS "x1"
  $ Quantifier FORALL "x2"
  $ Quantifier EXISTS "x3"
  $ Quantifier FORALL "x4"
  $ Relation "M" [Var "x1", Var "x2", Var "x3", Var "x4"]

--------------------------------------------------------------------------------
-- Herbrand universe
--------------------------------------------------------------------------------

herbrandUniverse
  :: forall c v
  .  [(c, Int)]
  -> [Term c v]
herbrandUniverse cs
  = map fst (concat (ts0 : generate 0 ts0 ts0))
  where
    cs0 = filter ((==0) . snd) cs
    cs' = filter ((>0) . snd) cs
    ts0 = map (\(c, 0) -> (Con c [], 0)) cs0

    generate
      :: Int
      -> [(Term c v, Int)]
      -> [(Term c v, Int)]
      -> [[(Term c v, Int)]]
    generate _ _ []
      = []
    generate n all last
      = new : generate (n + 1) (all ++ new) new
      where
        new = addLevel n all last

    addLevel
      :: Int
      -> [(Term c v, Int)]
      -> [(Term c v, Int)]
      -> [(Term c v, Int)]
    addLevel h all last
      = [ (Con c (map fst (ys +: y)), h + 1)
        | (c, n) <- cs'
        , ys <- timesN (n - 1) all
        , let h' = if n > 1 then maximum (map snd ys) else 0
        , y <- if h' < h then last else all
        ]

-- Ensure we have at least one constant with arity zero.
addArbitraryConstant :: [(c, Int)] -> [(Maybe c, Int)]
addArbitraryConstant cs
  | any ((==0) . snd) cs
    = cs'
  | otherwise
    = (Nothing, 0) : cs'
  where
    cs' = map (Just *** id) cs

quantifierFreeLines
  :: (Ord c, Ord v)
  => Formula Quantifier s r c v
  -> [Formula Quantifier s r (Maybe c) v]
quantifierFreeLines f
  = [ substFormula (Map.fromList (zip vs cs)) (mapConF Just f)
    | cs <- timesN (length vs) (herbrandUniverse cs)
    ]
  where
    vs = vars f
    cs = addArbitraryConstant (cons f)
