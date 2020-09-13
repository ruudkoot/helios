module Helios.Data.String
  ( module Data.String
  , bracket1
  , bracket2
  , padLeft
  , padRight
  ) where

import Data.String

bracket1 :: String -> String -> String
bracket1 bs ss = bs ++ ss ++ bs

bracket2 :: String -> String -> String -> String
bracket2 bs1 bs2 ss = bs1 ++ ss ++ bs2

padLeft :: Char -> Int -> String -> String
padLeft c n [] = replicate n c
padLeft c n (x:xs) = x : padLeft c (n-1) xs

padRight :: Char -> Int -> String -> String
padRight c n = reverse . padLeft c n . reverse