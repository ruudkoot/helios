module Helios.Math.Combinatorics
( factorial
, chooseOrdered
, choose
) where

factorial :: (Integral a) => a -> a
factorial n
  | 0 <= n
    = product [1 .. n]
  | otherwise
    = errorFactorial

-- FIXME: name?
chooseOrdered :: (Integral a) => a -> a -> a
n `chooseOrdered` k
  | 0 <= k, k <= n
    = product [n - k + 1 .. n]
  | otherwise
    = errorChoose

choose :: (Integral a) => a -> a -> a
choose n k
  | 0 <= k, k <= n
    = choose' n k
  | otherwise
    = errorChoose
  where
    choose' n 0
      = 1
    choose' n k
      | k > (n `div` 2)
        = choose' n (n-k)
      | otherwise
        = n * (choose' (n-1) (k-1)) `div` k

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

errorFactorial
  = error "errorFactorial"

errorChoose
  = error "errorChoose"