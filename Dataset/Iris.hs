{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Dataset.Iris
( load
) where

import Dataset

import Helios.Data.Relation.Dynamic
import Helios.Text.CSV as CSV

data Class
  = Iris_Setosa
  | Iris_Versicolor
  | Iris_Virginica
  deriving ( Eq, Ord, Show, Typeable )

parseClass :: String -> Class
parseClass "Iris-setosa"
  = Iris_Setosa
parseClass "Iris-versicolor"
  = Iris_Versicolor
parseClass "Iris-virginica"
  = Iris_Virginica
parseClass x
  = error x

options :: Options
options
  = Options
    { header
        = [ "_sepal length"
          , "_sepal width"
          , "_petal length"
          , "_petal width"
          , "_class"
          ]
    }

load :: IO Relation
load = do
  rel0 <- CSV.readFile options (datasetPath $__FILE__ "iris.data")
  let rel1
        = project
            ["sepal length","sepal width","petal length","petal width","class"]
        $ extend @Class ["_class"] "class" parseClass
        $ extend @Double ["_petal width"] "petal width" (read @Double)
        $ extend @Double ["_petal length"] "petal length" (read @Double)
        $ extend @Double ["_sepal width"] "sepal width" (read @Double)
        $ extend @Double ["_sepal length"] "sepal length" (read @Double)
        $ rel0
  return rel1
