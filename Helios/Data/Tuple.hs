module Helios.Data.Tuple
( dup
) where

dup :: a -> (a,a)
dup x = (x,x)