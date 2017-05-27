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
Module      : Monky.Examples.Wifi.Event
Description : The event based wifi module interface
Maintainer  : ongy
Stability   : experimental
Portability : Linux

This module provides the event based interface to wifi information.
It is rather limited because of some technical stuff with 802.11.
If you need more information about your wifi use the "Monky.Exmaple.Wifi.Poll" module.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
module Monky.Examples.Wifi.Event
  ( getWifiHandle
  , getWifiHandle'

  , getTextify

  , WifiEvtHandle
  , WifiFormat(..)
  )
where

import Formatting
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

import Monky.Modules
import Monky.Examples.Utility
import Monky.Wifi

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>), pure)
#endif

-- Socket Interface Conversion Offline
-- |The handle type for this module
data WifiEvtHandle = WH SSIDSocket Interface (WifiStats -> Text) Text

-- NOTE: If you extend this, also extend the type in Monky.Examples.Wifi.Poll
-- |A typesafe version of a format string
data WifiFormat
  = FormatChannel -- ^Print the current networks channel
  | FormatName -- ^Print the ESSID of the current network, may look weird because SSIDs are
  | FormatFreq -- ^Print the frequency the current network sends on (related to channel)
  | FormatText Text -- ^Print a plaintext string

-- | Apply the 'WifiFormat' to show some 'WifiStats' information as text.
getTextify :: WifiFormat -> WifiStats -> Text
getTextify FormatChannel    = sformat int . wifiChannel
getTextify FormatName       = T.pack . wifiName
getTextify FormatFreq       = sformat int . wifiFreq
getTextify (FormatText str) = const str

getFunction :: [WifiFormat] -> WifiStats -> Text
getFunction xs = T.concat . (\a -> map (($ a) . getTextify) xs)

-- |Get a wifi handle
getWifiHandle
  :: [WifiFormat] -- ^Format "String" for output generation
  -> Text -- ^Text that should be displayed when wifi is disconnected
  -> String -- ^Name of the interface
  -> IO WifiEvtHandle
getWifiHandle f d n =
  getWifiHandle' (getFunction f) d n

-- | Lower level version of 'getWifiHandle' if you need exted information.
getWifiHandle'
    :: (WifiStats -> Text)
    -> Text
    -> String
    -> IO WifiEvtHandle
getWifiHandle' f d n = do
    s <- getSSIDSocket
    i <- fromMaybe (error ("Could not find interface: " ++ n)) <$> getInterface s n
    return (WH s i f d)

getEventOutput :: WifiEvtHandle -> IO [MonkyOut]
getEventOutput (WH s i f d) = do
  new <- gotReadable s i
  case new of
    (WifiConnect x) -> do
        return [MonkyPlain $ f x]
    WifiNone -> return []
    WifiDisconnect -> return [MonkyPlain d]

instance EvtModule WifiEvtHandle where
  startEvtLoop h@(WH s i f d) r = do
    ret <- getCurrentWifiStats s i
    r . pure $ case ret of
            Nothing -> MonkyPlain d
            Just x -> MonkyPlain $ f x
    prepareEvents s
    loopFd h (getWifiFd s) r getEventOutput
