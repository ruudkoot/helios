module Helios.Math.Trig
( atan2'
, degrees
, inDegrees
, lawOfCosines
) where

atan2' :: RealFloat a => a -> a -> a
atan2' y x
  = let z = atan2 y x in if z < 0 then z + 2 * pi else z

--------------------------------------------------------------------------------
-- Angles
--------------------------------------------------------------------------------

degrees :: Floating a => a -> a
degrees x = x * (pi / 180.0)

inDegrees :: Floating a => a -> a
inDegrees x = x * (180.0 / pi)

--------------------------------------------------------------------------------
-- Law of Cosines: c^2 = a^2 + b^2 - 2ab cos(gamma)
--------------------------------------------------------------------------------

lawOfCosines :: Floating a => a -> a -> a -> a
lawOfCosines gamma a b
  = sqrt (a^2 + b^2 -2*a*b*cos(gamma))
