{-# OPTIONS_GHC -Wall #-}

-- TODO: what is primitive, what is not?
-- TODO: kerning etc. (pretty derivation trees may be hard)

module Helios.Logic.Doc
  ( Doc
  , (<->)
  , (<.>)
  , emptyDoc
  , doc
  , render
  ) where

import           Data.List

data Doc = Doc [String]

emptyDoc :: Doc
emptyDoc = Doc []

instance Show Doc where
  show = render

width :: Doc -> Int
width (Doc [])    = 0
width (Doc (d:_)) = length d

height :: Doc -> Int
height (Doc d) = length d

blank :: Int -> String
blank n = replicate n ' '

hline :: [String] -> [String] -> Int -> String
hline [] _  n = replicate n '═'
hline _  [] n = replicate n '─'
hline d1 d2 n = replicate l ' ' ++ replicate m '─' ++ replicate r ' '
  where s1 = last d1
        l1 = length (takeWhile (==' ') s1)
        r1 = length (takeWhile (==' ') (reverse s1))
        m1 = n - l1 - r1
        s2 = head d2
        l2 = length (takeWhile (==' ') s2)
        r2 = length (takeWhile (==' ') (reverse s2))
        m2 = n - l2 - r2
        (m,l,r) = if m1 > m2 then (m1,l1,r1) else (m2,l2,r2)

padTop :: Int -> [String] -> [String]
padTop n d =
  replicate (n - height (Doc d)) (blank (width (Doc d))) ++ d

padLeftRight :: Int -> String -> String
padLeftRight n ss = blank l ++ ss ++ blank r
  where m = n - length ss
        l = m `div` 2
        r = m - l

infix 7 <.>
infix 6 <->

(<->) :: Doc -> Doc -> Doc
Doc d1 <-> Doc d2 = Doc (map (padLeftRight n) (d1 ++ [hline d1 d2 n] ++ d2))
  where n = max (width (Doc d1)) (width (Doc d2))

(<.>) :: Doc -> Doc -> Doc
Doc d1 <.> Doc d2 =
  Doc (zipWith (\x y -> x ++ "  " ++ y) (padTop n d1) (padTop n d2))
  where n = max (height (Doc d1)) (height (Doc d2))

doc :: String -> Doc
doc ss = Doc [ss]

render :: Doc -> String
render (Doc d) = intercalate "\n" d
