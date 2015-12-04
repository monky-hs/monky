{-|
Module      : Monky.Examples.Wifi
Description : An example module instance for the wifi module
Maintainer  : ongy
Stability   : experimental
Portability : Linux

-}
module Monky.Examples.Wifi 
  ( getWifiHandle
  , WifiHandle
  )
where

import Data.IORef
import Data.Maybe (fromMaybe)
import System.Posix.Types (Fd)

import Monky.Modules
import Monky.Wifi

data WifiHandle = WH SSIDSocket (IORef String) Interface

getWifiHandle :: String -> IO WifiHandle
getWifiHandle n = do
  s <- getSSIDSocket
  i <- fromMaybe (error ("Could not find interface: " ++ n)) <$> getInterface s n
  r <- newIORef ""
  return (WH s r i)

getEventTextW :: Fd -> String -> WifiHandle -> IO String
getEventTextW _ _ (WH s r i) = do
  new <- gotReadable s i
  case new of
    (Just x) -> do
      writeIORef r x
      return x
    Nothing -> readIORef r

instance Module WifiHandle where
  getText _ (WH s _ i) = fromMaybe "None" <$> getCurrentWifi s i
  getEventText = getEventTextW
  getFDs (WH s _ _ ) = return [getWifiFd s]
