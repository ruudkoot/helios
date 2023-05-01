module Helios.Debug.Time
( time
, mapM_
) where

import Prelude hiding (mapM_)
import qualified GHC.Clock
-- import qualified Control.Concurrent

showTimeDiff :: Double -> Double -> String
showTimeDiff t0 t1
  =  show (t1 - t0) ++ "s"

time :: IO a -> IO a
time k = do
  t0 <- GHC.Clock.getMonotonicTime
  x <- k
  t1 <- GHC.Clock.getMonotonicTime
  print $ "Helios.Debug.Time.time: " ++ showTimeDiff t0 t1
  return x

mapM_ :: (a -> IO b) -> [a] -> IO ()
mapM_ _ [] = do
  return ()
mapM_ k (x:xs) = do
  t0 <- GHC.Clock.getMonotonicTime
  _y <- k x
  t1 <- GHC.Clock.getMonotonicTime
  print $ "Helios.Debug.Time.mapM_: " ++ showTimeDiff t0 t1
  mapM_ k xs