{-|
Module      : Monky.Examples.Network
Description : An example module instance for the network module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Network 
  ( getNetworkHandles'
  )
where


import Monky.Utility
import Monky.Modules
import Monky.Network

data NetworkHandles' = NH' String NetworkHandles

instance Module NetworkHandles' where
  getText u (NH' e h) = getNetworkText e u h

getNetworkHandles' :: String -> [String] -> IO NetworkHandles'
getNetworkHandles' e = fmap (NH' e) . getNetworkHandles

{- Network Module -}
formatNetworkText :: String -> Maybe (Int, Int) -> String
formatNetworkText e Nothing = e
formatNetworkText _ (Just (r, w)) =
  (convertUnit r  "B" "k" "M" "G") ++ ' ':(convertUnit w "B" "k" "M" "G")


getNetworkText :: String -> String -> NetworkHandles -> IO String
getNetworkText e _ nh = do
  nv <- getReadWriteMulti nh
  return (formatNetworkText e nv)


-- |Example instance for network module
instance Module NetworkHandles where
  getText = getNetworkText "Network Off"
