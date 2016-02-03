module Monky.Examples.Connectivity
  (
  )
where

import Monky.Modules
import Monky.Connectivity
import Data.IORef (readIORef)

instance Module ConnHandle where
  getText _ (ConnH _ _ ref)= do
    con <- readIORef ref
    return $show con
