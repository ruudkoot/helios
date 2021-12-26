module Helios.Visual.SVG
  ( module Helios.Visual.SVG.Types
  ) where

import System.FilePath
import System.IO

import Helios.Visual.SVG.Types

save :: FilePath -> SVG -> IO ()
save fp = writeFile fp . render . toXML

example1 :: SVG
example1
  = svg (width "391" <> height "391" <> viewBox (-70.5) (-70.5) 391 391)
    (
      rect (fill "#fff" <> stroke "#000" <> x "-70" <> y "-70" <> width "390" <> height "390") mempty
        <>
      g (opacity "0.8")
      (
        rect (x "25" <> y "25" <> width "200" <> height "200" <> fill "green" <> strokeWidth (cm 4) <> stroke "pink") mempty
          <>
        circle (cx "125" <> cy "125" <> r "75" <> fill "orange") mempty
          <>
        polyline (points [(50,150),(50,200),(200,200),(200,100)] <> stroke "red" <> strokeWidth (cm 4) <> fill "none") mempty
          <>
        line (x1 "50" <> y1 "50" <> x2 "200" <> y2 "200" <> stroke "blue" <> strokeWidth (cm 4)) mempty
      )
    )

main :: IO ()
main = do
  putStrLn (render (toXML example1))
  save ("helios" <.> fileNameExtension) example1
