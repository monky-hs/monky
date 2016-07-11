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
  , WifiFormat(..)
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

-- Socket (Ref,non-updates) Interface Conversion Offline
data WifiHandle = WH SSIDSocket (IORef String) Interface (WifiStats -> String) String

data WifiFormat
  = FormatChannel
  | FormatRates
  | FormatName
  | FormatFreq
  | FormatMBM
  | FormatText String

getFun :: WifiFormat -> WifiStats -> String
getFun FormatChannel    = show . wifiChannel
getFun FormatRates      = flip convertUnitSI "B" . maximum . wifiRates
getFun FormatName       = wifiName
getFun FormatFreq       = show . wifiFreq
getFun FormatMBM        = show . wifiMBM
getFun (FormatText str) = const str

getFunction :: [WifiFormat] -> WifiStats -> String
getFunction xs = concat . (\a -> map (($ a) . getFun) xs)

getWifiHandle :: [WifiFormat] -> String -> String -> IO WifiHandle
getWifiHandle f d n = do
  let fun = getFunction f
  s <- getSSIDSocket
  i <- fromMaybe (error ("Could not find interface: " ++ n)) <$> getInterface s n
  r <- newIORef ""
  return (WH s r i fun d)

writeAndRet :: IORef String -> String -> IO String
writeAndRet r str = writeIORef r str >> return str

getEventTextW :: Fd -> String -> WifiHandle -> IO String
getEventTextW _ _ (WH s r i f d) = do
  new <- gotReadable s i
  case new of
    (WifiConnect x) -> writeAndRet r $ f x
    WifiNone -> readIORef r
    WifiDisconnect -> writeAndRet r d

instance Module WifiHandle where
  getText _ (WH s r i f d) = do
    ret <- getCurrentWifiStats s i
    writeAndRet r $ maybe d f ret
  getEventText = getEventTextW
  getFDs (WH s _ _ _ _) = return [getWifiFd s]
