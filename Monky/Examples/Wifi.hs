{-|
Module      : Monky.Examples.Wifi
Description : An example module instance for the wifi module
Maintainer  : ongy
Stability   : experimental
Portability : Linux
-}
{-# LANGUAGE CPP #-}
module Monky.Examples.Wifi 
  ( getWifiHandle
  , WifiHandle
  )
where

import Data.IORef
import Data.Maybe (fromMaybe)
import System.Posix.Types (Fd)

import Monky.Modules
import Monky.Utility
import Monky.Wifi

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

data WifiHandle = WH SSIDSocket (IORef String) Interface

getWifiHandle :: String -> IO WifiHandle
getWifiHandle n = do
  s <- getSSIDSocket
  i <- fromMaybe (error ("Could not find interface: " ++ n)) <$> getInterface s n
  r <- newIORef ""
  return (WH s r i)

statToStr :: WifiStats -> String
statToStr x =
  let rs = wifiRates x
      mr = maximum rs in
    wifiName x ++ ':':(show $ wifiChannel x) ++ '@':(convertUnitSI mr "B")

getEventTextW :: Fd -> String -> WifiHandle -> IO String
getEventTextW _ _ (WH s r i) = do
  new <- gotReadable' s i
  case new of
    (Just x) -> do
      let str = statToStr x
      writeIORef r str
      return str
    Nothing -> readIORef r

instance Module WifiHandle where
  getText _ (WH s _ i) = do
    ret <- getCurrentWifiStats s i
    return $ fromMaybe "None" . fmap statToStr $ ret
  getEventText = getEventTextW
  getFDs (WH s _ _ ) = return [getWifiFd s]
