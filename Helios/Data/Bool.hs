module Helios.Data.Bool
( equal
) where

equal :: Eq a => [a] -> Bool
equal [] = True
equal (x:xs) = and (map (x==) xs)