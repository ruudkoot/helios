{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module BasketCDS where

import           Control.Monad
import           Data.List ( findIndex, intercalate )
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import           Data.Maybe

--------------------------------------------------------------------------------
-- Base types
--------------------------------------------------------------------------------

newtype Tenor = Y { toYear :: Double }
  deriving (Eq, Ord, Show)

yearDiff :: Tenor -> Tenor -> Double
yearDiff (Y y1) (Y y2)
  = y2 - y1

--------------------------------------------------------------------------------
-- Rate curves
--------------------------------------------------------------------------------

data RateCurve
  = RateCurve { rc_pillars :: Map Tenor Double }
  deriving (Eq, Show)

-- FIXME: we do not interpolate at all
-- FIXME: log interpolation instead of linear?
df :: RateCurve -> Tenor -> Double
df RateCurve{..} tenor
  = fromMaybe (error "df: tenor does not fall on a pillar")
  $ Map.lookup tenor rc_pillars

-- FIXME: rates instead of discount factors
rcUSD :: RateCurve
rcUSD
  = RateCurve $ Map.fromList
    [ (Y 0.00, 1.000000)
    , (Y 0.25, 0.986728)
    , (Y 0.50, 0.973296)
    , (Y 0.75, 0.960376)
    , (Y 1.00, 0.948345)
    , (Y 1.25, 0.936941)
    , (Y 1.50, 0.926686)
    , (Y 1.75, 0.917417)
    , (Y 2.00, 0.908656)
    , (Y 2.25, 0.900293)
    , (Y 2.50, 0.892159)
    , (Y 2.75, 0.884421)
    , (Y 3.00, 0.876818)
    , (Y 3.25, 0.869388)
    , (Y 3.50, 0.862203)
    , (Y 3.75, 0.855334)
    , (Y 4.00, 0.848469)
    , (Y 4.25, 0.841605)
    , (Y 4.50, 0.834856)
    , (Y 4.75, 0.828252)
    , (Y 5.00, 0.821676)
    ]

rcKannan1, rcKannan2, rcKannan3 :: RateCurve
rcKannan1
  = RateCurve $ Map.fromList
    [ (Y 0, 1.00)
    , (Y 1, 0.97)
    , (Y 2, 0.94)
    , (Y 3, 0.92)
    , (Y 4, 0.89)
    , (Y 5, 0.86)
    ]
rcKannan2
  = RateCurve $ Map.fromList
    [ (Y 0, 1.0000)
    , (Y 1, 0.9803)
    , (Y 2, 0.9514)
    , (Y 3, 0.9159)
    , (Y 4, 0.8756)
    , (Y 5, 0.8328)
    ]
rcKannan3
  = RateCurve $ Map.fromList
    [ (Y 0, 1.0000)
    , (Y 1, 0.9972)
    , (Y 2, 0.9916)
    , (Y 3, 0.9775)
    , (Y 4, 0.9619)
    , (Y 5, 0.9426)
    ]
rcKannan4
  = rcKannan3

--------------------------------------------------------------------------------
-- Single-name credit spreads
--------------------------------------------------------------------------------

data CreditSpread
  = CreditSpread { cs_pillars :: Map Tenor Double }
  deriving (Show)

pillars :: CreditSpread -> [Tenor]
pillars CreditSpread{..}
  = Map.keys cs_pillars

creditSpread :: CreditSpread -> Tenor -> Double
creditSpread CreditSpread{..} tenor
  = fromMaybe (error "creditSpread: tenor does not fall on a pillar")
  $ Map.lookup tenor cs_pillars

deltaT :: CreditSpread -> Tenor -> Double
deltaT cs tenor
  | n == 0
    = toYear tenor
  | otherwise
    = toYear tenor - toYear (pillars cs !! (n - 1))
  where
    n = fromMaybe (error $ "deltaT: not a pillar " ++ show tenor)
      $ findIndex (== tenor) (pillars cs)

csHSBC, csBNPParibas, csSantander, csUBS, csDeutscheBank
  :: CreditSpread
csHSBC
  = CreditSpread $ Map.fromList
    [ (Y 1.0, 0.003284)
    , (Y 2.0, 0.003907)
    , (Y 3.0, 0.004408)
    , (Y 4.0, 0.004980)
    , (Y 5.0, 0.005575)
    ]
csBNPParibas
  = CreditSpread $ Map.fromList
    [ (Y 1.0, 0.003500)
    , (Y 2.0, 0.004348)
    , (Y 3.0, 0.005080)
    , (Y 4.0, 0.005760)
    , (Y 5.0, 0.006418)
    ]
csSantander
  = CreditSpread $ Map.fromList
    [ (Y 1.0, 0.002854)
    , (Y 2.0, 0.003461)
    , (Y 3.0, 0.004303)
    , (Y 4.0, 0.005031)
    , (Y 5.0, 0.005688)
    ]
csUBS
  = CreditSpread $ Map.fromList
    [ (Y 1.0, 0.005124)
    , (Y 2.0, 0.006063)
    , (Y 3.0, 0.006669)
    , (Y 4.0, 0.007149)
    , (Y 4.0, 0.007574)
    ]
csDeutscheBank
  = CreditSpread $ Map.fromList
    [ (Y 1.0, 0.008574)
    , (Y 2.0, 0.009352)
    , (Y 3.0, 0.010089)
    , (Y 4.0, 0.010959)
    , (Y 5.0, 0.011833)
    ]

csKannan1, csKannan2, csKannan3
  :: CreditSpread
csKannan1
  = CreditSpread $ Map.fromList
    [ (Y 1.0, 0.00500)
    , (Y 2.0, 0.00770)
    , (Y 3.0, 0.00940)
    , (Y 4.0, 0.01095)
    , (Y 5.0, 0.01250)
    ]
csKannan2 -- FIXME: recoveryRate = 0.50
  = CreditSpread $ Map.fromList
    [ (Y 1.0, 0.00290)
    , (Y 2.0, 0.00390)
    , (Y 3.0, 0.00460)
    , (Y 4.0, 0.00520)
    , (Y 5.0, 0.00570)
    ]
csKannan3
  = CreditSpread $ Map.fromList
    [ (Y 1.0, 0.00112)
    , (Y 2.0, 0.00277)
    , (Y 3.0, 0.00369)
    , (Y 4.0, 0.00571)
    , (Y 5.0, 0.00678)
    ]
csKannan4
  = CreditSpread $ Map.fromList
    [ (Y 1.0, 0.00177)
    , (Y 2.0, 0.00446)
    , (Y 3.0, 0.00548)
    , (Y 4.0, 0.00835)
    , (Y 5.0, 0.00962)
    ]

--------------------------------------------------------------------------------
-- Credit Default Swap bootstrapping
--------------------------------------------------------------------------------

-- hazardRate   lambda
-- maturity     T
-- time         t
homogeneousSurvivalProbability hazardRate time maturity
  = exp (-hazardRate * (maturity - time))

homogeneousDefaultProbability hazardRate time maturity
  = 1 - homogeneousSurvivalProbability hazardRate time maturity

plot :: [Double] -> [Double -> Double] -> IO ()
plot xs fs
  = forM_ xs $ \x -> do
      putStrLn $ show x ++ "," ++ intercalate "," (map (show . ($ x)) fs)

plot_survivalProbability :: IO ()
plot_survivalProbability
  = plot [0.00, 0.01 .. 5.00] $
      map (\lambda -> homogeneousSurvivalProbability lambda 0)
        [0.01, 0.05, 0.10]

--------------------------------------------------------------------------------

data SurvivalCurve
  = SurvivalCurve { sc_pillars :: Map Tenor Double }
  deriving (Show)

recoveryRate :: Double
recoveryRate = 0.40

bootstrap :: RateCurve -> CreditSpread -> SurvivalCurve
bootstrap rc cs
  = SurvivalCurve { sc_pillars = Map.fromList (tail (zip tenors survivals)) }
  where
    lossRate
      = 1 - recoveryRate
    tenors@(tenor0:tenor1:_)
      = Y 0 : pillars cs
    deltaTs@(deltaT0:deltaT1:_)
      = undefined : map (deltaT cs) (tail tenors)
    survival0
      = 1
    survival1
      = lossRate / (lossRate + deltaT1 * creditSpread cs tenor1)
    survivals
      = survival0 : survival1 : map survival [2 .. length tenors - 1]
      where
        survival n
          = sum
            [ df rc (tenors !! i)
                *
              (
                lossRate * survivals !! (i-1)
                  -
                (lossRate + deltaTs !! i * creditSpread cs (tenors !! n))
                  *
                survivals !! i
              )
            | i <- [1 .. n-1]
            ]
              /
            (
              df rc (tenors !! n)
                *
              (lossRate + deltaTs !! n * creditSpread cs (tenors !! n))
            )
          +
            (survivals !! (n-1) * lossRate)
              /
            (lossRate + deltaTs !! n * creditSpread cs (tenors !! n))

--------------------------------------------------------------------------------
-- Basket CDS
--------------------------------------------------------------------------------

data BasketCDS
  = BasketCDS
    { kthToDefault :: Int
    , underlyings :: [String]
    }
