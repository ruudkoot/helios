module Helios.Optimize.VehicleRouting.Example.Whizzkids96
( main
) where

import qualified Dataset.Whizzkids96

import           Helios.Data.List
import           Helios.Optimize.VehicleRouting

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

status :: Problem -> String
status problem
  = concat
    [ "Value = ",    show (evaluate problem),             "; "
    , "Unplanned: ", show (length (routes problem !! 0)), "; "
    , "Route1 = ",   show (er 1),                         "; "
    , "Route2 = ",   show (er 2),                         "; "
    , "Route3 = ",   show (er 3),                         "; "
    , "Route4 = ",   show (er 4)
    ]
  where
    er n = evaluateRoute (depot problem) (routes problem !! n)

printExcel :: Problem -> IO ()
printExcel problem = do
  let table
        = map (intercalate ",")
        $ map (concatMap (maybe ["",""] (\(Loc x y) -> [show x, show y])))
        $ transpose
        $ pad Nothing
        $ map (map Just)
        $ routes problem
  mapM_ putStrLn table

main :: IO ()
main = do
  inst <- Dataset.Whizzkids96.load
  -- let prob = mkProblem inst
  prob <- mkProblemRandomStart inst
  let probs = localSearch prob
  mapM_ (\prob -> putStrLn (status prob) >> print prob) probs
  let sol = last probs
  printExcel sol