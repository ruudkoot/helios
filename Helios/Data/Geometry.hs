{-# LANGUAGE OverloadedLists #-}
module Helios.Data.Geometry
( Polar(..)
, toPolar
, fromPolar
, Cylindrical(..)
, toCylindrical
, fromCylindrical
, Spherical(..)
, toSpherical
, fromSpherical
, Plane(..)
, planeNormalPoint
) where

import Helios.Data.Vector
import Helios.Math.Trig

--------------------------------------------------------------------------------
-- Coordinates
--------------------------------------------------------------------------------

showAngle :: (Floating a, Show a) => a -> String
showAngle x  = show (x / pi) ++ "π"

data Polar a
  = Polar
    { polarR :: !a
    , polarT :: !a
    }

instance (Floating a, Show a) => Show (Polar a) where
  show (Polar r t)
    = "(" ++ show r ++ "," ++ showAngle t ++ ")"

toPolar :: RealFloat a => Vector a -> Polar a
toPolar v@[x,y]
  = Polar (sqrt (x * x + y * y)) (atan2' y x)
toPolar _
  = errorDimNot2

fromPolar :: Floating a => Polar a -> Vector a
fromPolar (Polar r t)
  = [ r * cos t, r * sin t ]

data Cylindrical a
  = Cylindrical
    { cylindricalR :: !a
    , cylindricalT :: !a
    , cylindricalZ :: !a
    }

instance (Floating a, Show a) => Show (Cylindrical a) where
  show (Cylindrical r t z)
    = "(" ++ show r ++ "," ++ showAngle t ++ "," ++ show z ++ ")"

toCylindrical :: RealFloat a => Vector a -> Cylindrical a
toCylindrical v@[x,y,z]
  = Cylindrical (sqrt (x * x + y * y)) (atan2' y x) z
toCylindrical _
  = errorDimNot3

fromCylindrical :: Floating a => Cylindrical a -> Vector a
fromCylindrical (Cylindrical r t z)
  = [ r * cos t, r * sin t, z ]

data Spherical a
  = Spherical
    { sphericalR :: !a
    , sphericalT :: !a
    , sphericalP :: !a
    }

instance (Floating a, Show a) => Show (Spherical a) where
  show (Spherical r t p)
    = "(" ++ show r ++ "," ++ showAngle t ++ "," ++ showAngle p ++ ")"

toSpherical :: RealFloat a => Vector a -> Spherical a
toSpherical v@[x,y,z]
  = Spherical r (atan2' y x) (acos (z / r))
  where r = sqrt (x * x + y * y + z * z)
toSpherical _
  = errorDimNot3

fromSpherical :: Floating a => Spherical a -> Vector a
fromSpherical (Spherical r t p)
  = [ r * sin p * cos t, r * sin p * sin t, r * cos p ]

--------------------------------------------------------------------------------
-- Planes
--------------------------------------------------------------------------------

data Plane a
  = Plane
    { planeNormal :: Vector a
    , planeDistance :: a
    }

instance Show a => Show (Plane a) where
  show plane
    = show (planeNormal plane) ++ "x+" ++ show (planeDistance plane) ++ "=0"

planeNormalPoint :: Num a => Vector a -> Vector a -> Plane a
planeNormalPoint v p
  | dim v == dim p
    = Plane
      { planeNormal
          = v
      , planeDistance
          = -(dotProduct v p)
      }
  | otherwise
    = errorDimNotEq

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

errorDimNot2
  = error "errorDimNot2"

errorDimNot3
  = error "errorDimNot3"
