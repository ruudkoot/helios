{-# OPTIONS_GHC -Wall #-}

module Helios.Logic.Parser
  ( Parser
  , parse
  , string
  , variable
  , uninterpretedConstant
  ) where

import           Control.Applicative
import           Data.Char

--------------------------------------------------------------------------------
-- Back-tracking parsers
--------------------------------------------------------------------------------

newtype Parser t a
  = Parser { runParser :: [t] -> [([t],a)] }

instance Functor (Parser t) where
  fmap f x = pure f <*> x

instance Applicative (Parser t) where
  p <*> q =
    Parser $ \ss ->
      [ (ss'', b2a b)
      | (ss',  b2a  ) <- runParser p ss
      , (ss'', b    ) <- runParser q ss'
      ]
  pure a = Parser (\s -> [(s,a)])

instance Alternative (Parser t) where
  p <|> q = Parser $ \ss -> runParser p ss ++ runParser q ss
  empty = Parser $ const []

instance Monad (Parser t) where
  return = pure
  Parser pa >>= a2pb =
    Parser $ \input ->
      [ b_input''
      | (input',a) <- pa input
      , b_input'' <- runParser (a2pb a) input'
      ]

string :: (Eq t) => [t] -> Parser t [t]
string k =
  Parser $ \xs -> case () of
    () | k == take n xs -> [(drop n xs, k)]
    () | otherwise -> []
    where n = length k

variable :: Parser Char String
variable =
  Parser $ \inp ->
    case inp of
      (s:ss) | isUpper s || isGreek s -> [(ss,[s])]
      _ -> []

uninterpretedConstant :: Parser Char String
uninterpretedConstant =
  Parser $ \inp ->
    case inp of
      (s:ss) | isLower s, not (isGreek s) -> [(ss,[s])]
      _ -> []

isGreek :: Char -> Bool
isGreek c = c `elem` ("αβγδεζηθικλμνξοπρστυφχψωΓΔΘΛΞΠΣΦΨΩ" :: String)

data ParseError
  = NoParse
-- | TrailingGarbage
  | Ambiguous
  deriving ( Eq, Ord, Show )

parse :: Parser Char a -> [Char] -> Either ParseError a
parse p s =
  case filter (null . fst) (runParser p (removeSpaces s)) of
    [] ->
      Left NoParse
    [([],x)] ->
      Right x
--  [(_,_)] ->
--    Left TrailingGarbage
    _ ->
      Left Ambiguous

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (' ':ss) = removeSpaces ss
removeSpaces (s:ss) = s : removeSpaces ss
