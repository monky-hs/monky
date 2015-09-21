{-|
Module      : Monky.Examples.Network
Description : An example module instance for the network module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Network ()
where

import Text.Printf(printf)

import Monky.Utility
import Monky.Modules
import Monky.Network

{- Network Module -}
formatNetworkText :: Maybe (Int, Int) -> String
formatNetworkText Nothing =
  printf "Network: off" :: String
formatNetworkText (Just (r, w)) =
  printf "%s %s" (convertUnit r  "B" "k" "M" "G") (convertUnit w "B" "k" "M" "G") :: String

getNetworkText :: String -> NetworkHandles -> IO String
getNetworkText _ nh = do
  nv <- getReadWriteMulti nh
  return (formatNetworkText nv)


-- |Example instance for network module
instance Module NetworkHandles where
  getText = getNetworkText
