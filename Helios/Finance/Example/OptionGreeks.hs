{-# LANGUAGE OverloadedStrings #-}
module Helios.Finance.Example.OptionGreeks
( main
) where

import           Helios.Data.Relation.Dynamic
import qualified Helios.Finance.Instrument        as Instrument
import qualified Helios.Finance.Instrument.Option as Option
import qualified Helios.Finance.Market            as Market
import qualified Helios.Finance.Price             as Price
import qualified Helios.Finance.Risk              as Risk

market :: Market.Market
market
  = Market.Market
    { Market.rate =   0.05
    , Market.spot = 100.00
    , Market.vol  =   0.20
    }

options :: [Option.Option]
options
  = [ Option.Option Option.European Option.Call 100 1
    , Option.Option Option.European Option.Put  100 1
    , Option.Option Option.American Option.Call 100 1
    , Option.Option Option.American Option.Put  100 1
    ]

mkRow
  :: Market.Market
  -> Option.Option
  -> (Option.Option,Double,Double,Double,Double,Double,Double)
mkRow market option
  = ( option
    , Price.value taylor
    , Price.delta taylor
    , Price.gamma taylor
    , Risk.vega market option
    , Option.theta market option
    , Risk.rho market option
    )
  where
    taylor = Instrument.price market option

header :: [Attribute]
header = ["Instrument","Value","Delta","Gamma","Vega","Theta","Rho"]

main :: IO ()
main = do
  print market
  let rel = relationR header $ map (mkRow market) options
  print rel
