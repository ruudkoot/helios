{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Copula where

import Control.Monad
import Data.List ( intercalate )
import System.Random

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

positiveInfinity :: Double
positiveInfinity = 1/0

negativeInfinity :: Double
negativeInfinity = -1/0

--------------------------------------------------------------------------------
-- Standard normal cumulative distribution function
--------------------------------------------------------------------------------

-- Abramowitz and Stegun formula 7.1.26
-- https://www.johndcook.com/blog/haskell-phi/
normalCDF
  :: Double -> Double
normalCDF x
  = 0.5 * (s * e + 1.0)
  where
    s = if x > 0 then 1 else -1
    t = 1.0/(1.0 + p * abs x / sqrt 2.0)
    e = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x/2.0)

    a1 =  0.254829592
    a2 = -0.284496736
    a3 =  1.421413741
    a4 = -1.453152027
    a5 =  1.061405429
    p  =  0.3275911

--------------------------------------------------------------------------------
-- Inverse standard normal cumulative distribution function
--------------------------------------------------------------------------------

-- Peter John Acklam. "An algorithm for computing the inverse normal cumulative
-- distribution function"
inverseNormalCDF
  :: Double -> Double
inverseNormalCDF 0
  = negativeInfinity
inverseNormalCDF 1
  = positiveInfinity
inverseNormalCDF p
  | 0 < p, p < p_low
    = let q = sqrt ((-2) * log p)
      in (((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6)
          / ((((d1*q+d2)*q+d3)*q+d4)*q+1)
  | p_low <= p, p <= p_high
    = let q = p - 0.5
          r = q*q
      in (((((a1*r+a2)*r+a3)*r+a4)*r+a5)*r+a6)*q
          / (((((b1*r+b2)*r+b3)*r+b4)*r+b5)*r+1)
  | p_high < p, p < 1
    = let q = sqrt ((-2)*log (1-p))
      in -(((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6)
          / ((((d1*q+d2)*q+d3)*q+d4)*q+1)
  where
    p_low  = 0.02425
    p_high = 1 - p_low
    a1 = -3.969683028665376e+01
    a2 =  2.209460984245205e+02
    a3 = -2.759285104469687e+02
    a4 =  1.383577518672690e+02
    a5 = -3.066479806614716e+01
    a6 =  2.506628277459239e+00
    b1 = -5.447609879822406e+01
    b2 =  1.615858368580409e+02
    b3 = -1.556989798598866e+02
    b4 =  6.680131188771972e+01
    b5 = -1.328068155288572e+01
    c1 = -7.784894002430293e-03
    c2 = -3.223964580411365e-01
    c3 = -2.400758277161838e+00
    c4 = -2.549732539343734e+00
    c5 =  4.374664141464968e+00
    c6 =  2.938163982698783e+00
    d1 =  7.784695709041462e-03
    d2 =  3.224671290700398e-01
    d3 =  2.445134137142996e+00
    d4 =  3.754408661907416e+00
inverseNormalCDF p
  = error "inverseNormalCDF: p not in [0,1]"

--------------------------------------------------------------------------------
-- Inverse exponential cumulative distribution function
--------------------------------------------------------------------------------

inverseExponentialCDF
  :: Double -> Double -> Double
inverseExponentialCDF lambda u
  = -log (1 - u) / lambda

--------------------------------------------------------------------------------
-- Gaussian copula
--------------------------------------------------------------------------------

data DefaultTime
  = DefaultTime
    { w1, w2      :: Double -- uniform, independent
    , z1, z2      :: Double -- normal, independent
    , x1, x2      :: Double -- normal, correlated
    , u1, u2      :: Double -- uniform, correlated
    , tau1, tau2  :: Double -- exponential, correlated
    }

drawDefaultTime :: Double -> Double -> IO DefaultTime
drawDefaultTime lambda rho = do
  w1 <- randomIO                                    -- Sobel numbers
  w2 <- randomIO
  let z1   = inverseNormalCDF w1                    -- t-distribution?
      z2   = inverseNormalCDF w2
      x1   = z1                                     -- Cholesky decomposition
      x2   = rho * z1 + sqrt (1 - rho * rho) * z2
      u1   = normalCDF x1
      u2   = normalCDF x2
      tau1 = inverseExponentialCDF lambda u1
      tau2 = inverseExponentialCDF lambda u2
  return DefaultTime{..}

main :: IO ()
main = do
  let lambda = 0.2
      rho    = 0.8
  dts <- replicateM 10000 (drawDefaultTime lambda rho)
  putStrLn "w1,w2,z1,z2,x1,x2,u1,u2,tau1,tau2"
  forM_ dts $ \DefaultTime{..} -> do
    putStrLn $ intercalate "," (map show [w1,w2,z1,z2,x1,x2,u1,u2,tau1,tau2])
