{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-

https://web.archive.org/web/19990501123247/http://www.win.tue.nl/whizzkids/1996/index_ned.html

-}
module Dataset.Whizzkids96
( load
) where

import Dataset

import Helios.Optimize.VehicleRouting.Types

load :: IO (Loc, [Loc])
load = do
  let parse = (\[_,x,y] -> Loc (read x) (read y)) . words
  (x:xs) <- map parse . take 121 . drop 5 . lines
    <$> readFile (datasetPath $__FILE__ "instance.txt")
  return (x,xs)