{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Helios.Logic.Term
  ( Term(..)
  , vars
  , Sig(..)
  , signature
  , SIG(..)
  , arity
  , groundTerms
  , Unif(..)
  , apply
  , match
  ) where

import           Control.Applicative
import           Data.Either
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.String

import Helios.Logic.Data
import Helios.Logic.Parser
import Helios.Logic.Infinite

--------------------------------------------------------------------------------
-- First-order terms
--------------------------------------------------------------------------------

-- TODO: Name = Int
type Name = Either String Int

-- NOTE: Con Left implies
data Term sig
  = Var String
  | Con Name [Term sig]
  deriving ( Eq, Ord )

vars :: Term sig -> [String]
vars (Var x) = [x]
vars (Con _ ts) = concatMap vars ts

--------------------------------------------------------------------------------
-- Signatures
--------------------------------------------------------------------------------

-- TODO: sorts
-- TODO: Wieland, Jacob (2009). Parsing Mixfix Expressions

data Sig sig
  = Sig
    { sorts :: ()
    , operators :: [Operator]
    }
  deriving ( Eq, Show )

-- TODO: uninterpreted ocnstatns
signature :: [String] -> Sig sig
signature = Sig () . map operator

class SIG sig where
  _SIG_ :: Sig sig

instance SIG sig => IsString (Term sig) where
  fromString = either (error . show) id . parse (signatureParser _SIG_)

instance SIG sig => Show (Term sig) where
  show = signaturePrinter _SIG_

data Token
  = Text String
  | Hole {- Sort -}
  deriving ( Eq, Show )

tokenParser
  :: (Parser Char hole)
  -> Token
  -> Parser Char (Either String hole)
tokenParser _ (Text s) =
  Left <$> string s
tokenParser holeParser Hole =
  Right <$> holeParser

newtype Operator
  = Operator
    { tokens :: [Token]
    }
  deriving ( Eq, Show )

-- FIXME: store in signature
arity :: Operator -> Int
arity (Operator []) = 0
arity (Operator (Text _ : ts)) = arity (Operator ts)
arity (Operator (Hole : ts)) = 1 + arity (Operator ts)

operator :: String -> Operator
operator = Operator . operator'
  where operator' :: String -> [Token]
        operator' [] =
          []
        operator' (' ' : ss) =
          operator' ss
        operator' ('_' : ss) =
          Hole : operator' ss
        operator' (s : ss) =
          case operator' ss of
            [] ->
              [Text [s]]
            Text t : ts ->
              Text (s : t) : ts
            Hole : ts ->
              Text [s] : Hole : ts

signatureParser :: Sig sig -> Parser Char (Term sig)
signatureParser (Sig () operators) =
  operatorParser highestPrecedence
  where highestPrecedence = length operators - 1
        operatorParser :: Int -> Parser Char (Term sig)
        operatorParser (-1) =
          (Var <$> variable)
            <|>
          ((\x -> Con (Left x) []) <$> uninterpretedConstant)
        operatorParser n =
          mkParser n (holeParser n) (operators !! n)
            <|>
          operatorParser (n-1)
        holeParser :: Int -> Parser Char (Term sig)
        holeParser n =
          ((\_ x _ -> x) <$> string "("
                         <*> operatorParser highestPrecedence
                         <*> string ")"
          )
            <|>
          (operatorParser (n - 1))

mkParser :: Int -> Parser Char (Term sig) -> Operator -> Parser Char (Term sig)
mkParser n holeParser (Operator tokens) = do
  xs <- mapM (tokenParser holeParser) tokens
  return (Con (Right n) (rights xs))

signaturePrinter :: Sig sig -> Term sig -> String
signaturePrinter (Sig () operators) term =
  printTerm infintePrecedence term
  where infintePrecedence = length operators

        printTerm :: Int -> Term sig -> String
        printTerm _ (Var x) =
          x
        printTerm _ (Con (Left n) []) =
          n
        printTerm m (Con (Right n) ts) =
          prec m n (printTokens n (tokens (operators !! n)) ts)
        printTerm _ _ =
          error "printTerm"

        printTokens :: Int -> [Token] -> [Term sig] -> String
        printTokens _ [] [] =
          []
        printTokens n (Text s : ss) ts =
          s ++ printTokens n ss ts
        printTokens n (Hole : ss) (t : ts) =
          printTerm n t ++ printTokens n ss ts
        printTokens _ _ _ =
          error "printTokens"

prec :: Int -> Int -> String -> String
prec n m ss
  | n <= m = "(" ++ ss ++ ")"
  | otherwise = ss

--------------------------------------------------------------------------------
-- Ground terms
--------------------------------------------------------------------------------

data TEST

test :: Sig TEST
test =
  signature
  [ "f{_,_}"
  ]

instance SIG TEST where
  _SIG_ = test

termHeight :: Term sig -> Int
termHeight (Var _) = 1
termHeight (Con _ ts) = 1 + maximum (0 : map termHeight ts)

clist :: Sig sig -> [(Int,Int)]
clist (Sig () ops) = zipWith (\op n -> (n, arity op)) ops [0..]

clist0 :: Sig sig -> [Term sig]
clist0 = map (\(c,_) -> Con (Right c) []) . filter ((== 0) . snd) . clist

clist' :: Sig sig -> [(Int,Int)]
clist' = filter ((> 0) . snd) . clist

-- FIXME: inefficient (termHeight, filter)
-- FIXME: check uniqueness
groundTerms :: forall sig. SIG sig => [String] -> [[Term sig]]
groundTerms gc =
  zipWith (\n -> filter (\t -> termHeight t == n)) [1..] (reiterate grow c0)
  where c0 = clist0 (_SIG_ :: Sig sig) ++ map (\c -> Con (Left c) []) gc
        cs = clist' (_SIG_ :: Sig sig)

        grow :: [Term sig] -> [Term sig]
        grow ts = concatMap mk cs
          where mk (c,n) = map (Con (Right c)) (finiteCross (replicate n ts))

reiterate :: ([a] -> [a]) -> [a] -> [[a]]
reiterate f xs = xs : reiterate f (xs ++ f xs)

--------------------------------------------------------------------------------
-- Unifiers and matching
--------------------------------------------------------------------------------

newtype Unif sig = Unif (Map.Map String (Term sig))
  deriving ( Eq, Show )

identityUnif :: Unif sig
identityUnif = Unif Map.empty

singletonUnif :: String -> Term sig -> Unif sig
singletonUnif x t = Unif (Map.singleton x t)

compose :: Unif sig -> Unif sig -> Unif sig
compose (Unif u2) (Unif u1) | disjoint u1 u2 =
  Unif (Map.union u2 (Map.map (apply (Unif u2)) u1))
compose _ _ =
  error "compose"

apply :: Unif sig -> Term sig -> Term sig
apply (Unif unif) (Var x) =
  fromMaybe (Var x) (Map.lookup x unif)
apply unif (Con x ts) =
  Con x (map (apply unif) ts)

match :: Term sig -> Term sig -> Maybe (Unif sig)
match (Var x) t =
  Just (singletonUnif x t)
match (Con c1 ts1) (Con c2 ts2) | c1 == c2 =
  matchList ts1 ts2
match _ _ =
  Nothing

matchList :: [Term sig] -> [Term sig] -> Maybe (Unif sig)
matchList [] [] =
  Just identityUnif
matchList (x:xs) (y:ys) =
  do
    u1 <- match x y
    u2 <- matchList (map (apply u1) xs) (map (apply u1) ys)
    return (compose u2 u1)
matchList _ _ =
  error "matchList"
