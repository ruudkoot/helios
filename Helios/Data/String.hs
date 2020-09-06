module Helios.Data.String
  ( padLeft
  , padRight
  ) where

padLeft :: Char -> Int -> String -> String
padLeft c n [] = replicate n c
padLeft c n (x:xs) = x : padLeft c (n-1) xs

padRight :: Char -> Int -> String -> String
padRight c n = reverse . padLeft c n . reverse