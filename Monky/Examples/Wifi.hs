{-
    Copyright 2016 Markus Ongyerth

    This file is part of Monky.

    Monky is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Monky is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Monky.  If not, see <http://www.gnu.org/licenses/>.
-}
{-|
Module      : Monky.Examples.Wifi
Description : An example module instance for the wifi module
Maintainer  : ongy
Stability   : experimental
Portability : Linux

FormatSignal only makes sense when used in a pollModule. But even in pollModule
context updates may take a while because of some buffering by drivers or
netlink subsystem.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Monky.Examples.Wifi
  ( getWifiHandle
  , getWifiHandle'

    -- TODO: rename
  , getFunction
  , doMBM

  , WifiHandle
  , WifiFormat(..)
  , WifiStats
  )
where

import Formatting
import Data.Word (Word32, Word8)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

import Monky.Modules
import Monky.Examples.Utility
import Monky.Wifi

import System.Linux.Netlink.GeNetlink.NL80211 (NL80211Packet)

import System.IO.Unsafe (unsafeInterleaveIO)

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

-- Socket Interface Conversion Offline
-- |The handle type for this module
data WifiHandle = WH SSIDSocket Interface ((WifiStats, Maybe NL80211Packet) -> Text) Text

-- |A typesafe version of a format string
data WifiFormat
  = FormatChannel -- ^Print the current networks channel
  | FormatRates -- ^Print the current network max supported rate (always 54Mbit/s for me)
  | FormatName -- ^Print the ESSID of the current network, may look weird because SSIDs are
  | FormatFreq -- ^Print the frequency the current network sends on (related to channel)
  | FormatSignal -- ^Print link quality (0-100)
  | FormatText Text -- ^Print a plaintext string

getFun :: WifiFormat -> WifiStats -> Text
getFun FormatChannel    = sformat int . wifiChannel
getFun FormatRates      = flip convertUnitSI "B" . maximum . wifiRates
getFun FormatName       = T.pack . wifiName
getFun FormatFreq       = sformat int . wifiFreq
getFun FormatSignal     = sformat int . doStrength . wifiSig
getFun (FormatText str) = const str

getFunction :: [WifiFormat] -> WifiStats -> Text
getFunction xs = T.concat . (\a -> map (($ a) . getFun) xs)

-- |Do the calculation for MBM
-- This is taken from NetworkManager
doMBM :: Word32 -> Word8
doMBM e =
  let noiseFloor = -90
      signalMax  = -20
      work :: Int32 = fromIntegral e
      clamped :: Float = min signalMax $ max noiseFloor $ fromIntegral $ work `div` 100
      in floor (100 - 70 * ((signalMax - clamped) / (signalMax - noiseFloor)))

-- Helper for FormatSignal that splits signal types
doStrength :: Signal -> Word8
doStrength (SigMBM mbm) = doMBM mbm
doStrength (SigUNSPEC unspec) = unspec

-- |Get a wifi handle
getWifiHandle
  :: [WifiFormat] -- ^Format "String" for output generation
  -> Text -- ^Text that should be displayed when wifi is disconnected
  -> String -- ^Name of the interface
  -> IO WifiHandle
getWifiHandle f d n =
  getWifiHandle' (getFunction f . fst) d n

-- | Lower level version of 'getWifiHandle' if you need exted information.
getWifiHandle'
    :: ((WifiStats, Maybe NL80211Packet) -> Text)
    -> Text
    -> String
    -> IO WifiHandle
getWifiHandle' f d n = do
    s <- getSSIDSocket
    i <- fromMaybe (error ("Could not find interface: " ++ n)) <$> getInterface s n
    return (WH s i f d)


getEventOutput :: WifiHandle -> IO [MonkyOut]
getEventOutput (WH s i f d) = do
  new <- gotReadable s i
  case new of
    (WifiConnect x) -> do
        ext <- unsafeInterleaveIO $ getExtendedWifi s i x
        return [MonkyPlain $ f (x, ext)]
    WifiNone -> return []
    WifiDisconnect -> return [MonkyPlain d]

instance EvtModule WifiHandle where
  startEvtLoop h@(WH s _ _ _) r = do
    r =<< getOutput h
    prepareEvents s
    loopFd h (getWifiFd s) r getEventOutput

instance PollModule WifiHandle where
  getOutput (WH s i f d) = do
    ret <- getCurrentWifiStats s i
    case ret of
      Nothing -> return [MonkyPlain d]
      Just x -> do
        ext <- unsafeInterleaveIO $ getExtendedWifi s i x
        return [MonkyPlain $ f (x, ext)]
