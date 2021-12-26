-- axis
-- points
-- vectors

-- TikZ/PGF

module Helios.Visual.Plot
  ( main
  ) where

import System.FilePath

import Helios.Visual.SVG
{-
plot :: SVG
plot
  = svg (width 500 <> height 500 <> viewBox -0.1 -0.1 1.2 1.2)
        (axis <> dots)
  where
    axis
      = line (x1 )
    dots
      =
-}
main = save ("plot" <.> fileNameExtension)


