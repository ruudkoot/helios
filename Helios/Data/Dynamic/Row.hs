module Helios.Data.Dynamic.Row
  ( Row(..)
  ) where

import Helios.Data.Dynamic.Types

class Row r where
  rowWidth :: Proxy r -> Int
  rowIndex :: Int -> r -> Packet

instance (Dynamic t1, Dynamic t2) => Row (t1,t2) where
  rowWidth _ = 2
  rowIndex 0 (x,_) = Packet x
  rowIndex 1 (_,x) = Packet x

instance (Dynamic t1, Dynamic t2, Dynamic t3) => Row (t1,t2,t3) where
  rowWidth _ = 3
  rowIndex 0 (x,_,_) = Packet x
  rowIndex 1 (_,x,_) = Packet x
  rowIndex 2 (_,_,x) = Packet x

instance (Dynamic t1, Dynamic t2, Dynamic t3, Dynamic t4)
            => Row (t1,t2,t3,t4) where
  rowWidth _ = 4
  rowIndex 0 (x,_,_,_) = Packet x
  rowIndex 1 (_,x,_,_) = Packet x
  rowIndex 2 (_,_,x,_) = Packet x
  rowIndex 3 (_,_,_,x) = Packet x

instance (Dynamic t1, Dynamic t2, Dynamic t3, Dynamic t4, Dynamic t5)
            => Row (t1,t2,t3,t4,t5) where
  rowWidth _ = 5
  rowIndex 0 (x,_,_,_,_) = Packet x
  rowIndex 1 (_,x,_,_,_) = Packet x
  rowIndex 2 (_,_,x,_,_) = Packet x
  rowIndex 3 (_,_,_,x,_) = Packet x
  rowIndex 4 (_,_,_,_,x) = Packet x
