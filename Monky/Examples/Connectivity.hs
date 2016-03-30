{-|
Module      : Monky.Examples.Connectivity
Description : Simple Connectivity example
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Connectivity
  (
  )
where

import Monky.Modules
import Monky.Connectivity

showCon :: Bool -> String
showCon False = "Unconnected"
showCon True  = "  Connected"

instance Module ConnHandle where
  getText _ = fmap showCon . hasConn
