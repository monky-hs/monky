{-|
Module      : Monky.Examples.Wifi
Description : An example module instance for the wifi module
Maintainer  : ongy
Stability   : experimental
Portability : Linux
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Monky.Examples.Wifi
  ( getWifiHandle
  , WifiHandle
  , WifiFormat(..)
  )
where

import Formatting
import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Data.Maybe (fromMaybe)

import Monky.Modules
import Monky.Examples.Utility
import Monky.Wifi

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

-- Socket Interface Conversion Offline
data WifiHandle = WH SSIDSocket Interface (WifiStats -> Text) Text

data WifiFormat
  = FormatChannel
  | FormatRates
  | FormatName
  | FormatFreq
  | FormatMBM
  | FormatText String

getFun :: WifiFormat -> WifiStats -> Text
getFun FormatChannel    = sformat int . wifiChannel
getFun FormatRates      = flip convertUnitSI "B" . maximum . wifiRates
getFun FormatName       = T.pack . wifiName
getFun FormatFreq       = sformat int . wifiFreq
getFun FormatMBM        = sformat int . wifiMBM
getFun (FormatText str) = const (T.pack str)

getFunction :: [WifiFormat] -> WifiStats -> Text
getFunction xs = T.concat . (\a -> map (($ a) . getFun) xs)

getWifiHandle :: [WifiFormat] -> Text -> String -> IO WifiHandle
getWifiHandle f d n = do
  let fun = getFunction f
  s <- getSSIDSocket
  i <- fromMaybe (error ("Could not find interface: " ++ n)) <$> getInterface s n
  return (WH s i fun d)

-- writeAndRet :: IORef String -> String -> IO String
-- writeAndRet r str = writeIORef r str >> return str
-- 
-- getEventTextW :: Fd -> String -> WifiHandle -> IO String
-- getEventTextW _ _ (WH s r i f d) = do
--   new <- gotReadable s i
--   case new of
--     (WifiConnect x) -> writeAndRet r $ f x
--     WifiNone -> readIORef r
--     WifiDisconnect -> writeAndRet r d

-- instance Module WifiHandle where
--   getText _ (WH s r i f d) = do
--     ret <- getCurrentWifiStats s i
--     writeAndRet r $ maybe d f ret
--   getEventText = getEventTextW
--   getFDs (WH s _ _ _ _) = return [getWifiFd s]

getEventOutput :: WifiHandle -> IO [MonkyOut]
getEventOutput (WH s i f d) = do
  new <- gotReadable s i
  case new of
    (WifiConnect x) -> return [MonkyPlain $ f x]
    WifiNone -> return []
    WifiDisconnect -> return [MonkyPlain d]

instance EvtModule WifiHandle where
  startEvtLoop h@(WH s _ _ _) r = do
    atomicWriteIORef r =<< getOutput h
    loopFd h (getWifiFd s) r getEventOutput

instance PollModule WifiHandle where
  getOutput (WH s i f d) = do
    ret <- getCurrentWifiStats s i
    return [MonkyPlain $ maybe d f ret]
