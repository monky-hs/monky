{-
    Copyright 2017 Markus Ongyerth

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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumDecimals #-}
{-|
Module      : Monky.Examples.Wifi.Poll
Description : The polling api for wifi information
Maintainer  : ongy
Stability   : experimental
Portability : Linux

This provides polling access to wifi information.
For the event based version look at "Monky.Examples.Wifi.Event"
-}
module Monky.Examples.Wifi.Poll
    ( WifiFormat (..)
    , WifiPollHandle
    , Direction(..)

    , getWifiHandle
    , getWifiHandle'
    )
where

import Debug.Trace

import Data.Int (Int8)

import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Formatting
import Monky.Examples.Utility
import qualified Monky.Examples.Wifi.Event as E
import Monky.Modules
import Monky.Wifi
import System.Linux.Netlink.GeNetlink.NL80211
import System.Linux.Netlink.GeNetlink.NL80211.StaInfo

import Data.Text (Text)
import qualified Data.Text as T

import Control.Applicative ((<|>), (<$>), (<*>), pure)

-- | The type for polling wifi information
data WifiPollHandle = WH SSIDSocket Interface ((WifiStats, Maybe NL80211Packet) -> Text) Text

-- TODO: Find out which of RX and TX is which from *our* side and document it
-- | Helper type for WifiFormat to specify direction
data Direction
    -- | Use TX information
    = DirTX
    -- | Use TX information
    | DirRX
    deriving (Show, Eq)

-- | Enum-ish type for converting Wifi information to text
data WifiFormat
    -- | The MCSIndex for our connection
    = FormatMCS Direction
    -- | The minimum MCSIndex for our connection
    | FormatMCSMin
    -- | The Signal width (in MHz)
    | FormatWifiWidth
    -- | The Bitrate of our connection
    | FormatBitrate Direction
    -- | Minimum of TX/RX Bitrate for this station
    | FormatBitrateMin
    -- | Signal strength from other source
    | FormatSignal
    -- | Signal strength average
    | FormatSignalAverage

    | FormatChannel -- ^Print the current networks channel
    | FormatName -- ^Print the ESSID of the current network, may look weird because SSIDs are
    | FormatFreq -- ^Print the frequency the current network sends on (related to channel)
    | FormatText Text -- ^Print a plaintext string
    deriving (Show, Eq)

-- |Do the calculation for MBM
-- This is taken from NetworkManager
doMBM :: Int8 -> Word8
doMBM e =
  let noiseFloor = -90
      signalMax  = -20
      clamped = min signalMax $ max noiseFloor $ e
   in fromIntegral $ 100 - (signalMax - clamped)

getFromDir :: Direction -> StaInfo -> Maybe StaRate
getFromDir DirTX = staTXRate
getFromDir DirRX = staRXRate

pollToEvt :: WifiFormat -> E.WifiFormat
pollToEvt FormatChannel  = E.FormatChannel
pollToEvt FormatName     = E.FormatName
pollToEvt FormatFreq     = E.FormatFreq
pollToEvt (FormatText t) = E.FormatText t
pollToEvt x = error $ "Tried to convert " ++ show x ++ "to Evt? This really shouldn't ever happen"

getExtFun :: WifiFormat -> (WifiStats, StaInfo) -> Text
getExtFun (FormatMCS dir) (_, info) =
    case getFromDir dir info of
        Nothing -> "No Rate"
        Just x -> case rateMCS x <|> rateVHTMCS x of
            Nothing -> "No MCS"
            Just y -> sformat int y
getExtFun FormatMCSMin (_, info) = fromMaybe "No Rate" $ do
    rx <- staRXRate info
    tx <- staTXRate info
    rmcs <- getMCS rx
    tmcs <- getMCS tx
    pure . sformat int $ min rmcs tmcs
    where getMCS x = rateMCS x <|> rateVHTMCS x
getExtFun FormatWifiWidth (_, info) = -- Width is also in TXRate
    case staTXRate info of
        Nothing -> "No Rate"
        Just x -> case rateWidthFlag x of
            Width5MHz -> "5MHz"
            Width10MHz -> "10MHz"
            Width20MHz -> "20MHz"
            Width40MHz -> "40MHz"
            Width80MHz -> "80MHz"
            Width160MHz -> "160MHz"
getExtFun (FormatBitrate dir) (_, info) = -- Bitrate is in TXRate
    case getFromDir dir info of
        Nothing -> "No Rate"
        Just x -> maybe
            "No Bitrate"
            (flip convertUnitSI "b" . (* 1e5))
            (rateBitrate x)
getExtFun FormatBitrateMin (_, info) = -- Bitrate from RX/TX Rate
    let tx = rateBitrate =<< staTXRate info
        rx = rateBitrate =<< staRXRate info
    in case min <$> rx <*> tx of
        Nothing -> "No rates"
        Just x -> convertUnitSI (x * 1e5) "b"
getExtFun FormatSignal (_, info) =
    case staSignalMBM info of
        Nothing -> "No strength"
        Just x -> sformat int . doMBM . traceShowId $ fromIntegral x
getExtFun FormatSignalAverage (_, info) =
    case staSignalMBMA info of
        Nothing -> "No strength"
        Just x -> sformat int . doMBM $ fromIntegral x
getExtFun x (stats, _) = E.getTextify (pollToEvt x) stats

getExtFunction :: [WifiFormat] -> (WifiStats, StaInfo) -> Text
getExtFunction xs = T.concat . (sequence . map getExtFun $ xs)

{- |This function is the easiest, but also a bit limited way to get extended wifi information.

With this, all "normal" information will be first, and the extended information will be appended.
-}
getCombiFun :: [WifiFormat] -> ((WifiStats, Maybe NL80211Packet) -> Text)
getCombiFun xs (stat, ext) =
    let fun = getExtFunction xs
        info = staInfoFromPacket =<< ext
     in case info of
            Just x -> fun (stat, x)
            Nothing -> "Couldn't get wifi station info"

instance PollModule WifiPollHandle where
  getOutput (WH s i f d) = do
    ret <- getCurrentWifiStats s i
    case ret of
        Nothing -> pure . pure $ MonkyPlain d
        Just x -> do
            ext <- getExtendedWifi s i x
            pure . pure . MonkyPlain $ f (x, ext)

-- | Lower level version of 'getWifiHandle' if you need exted information.
getWifiHandle'
    :: ((WifiStats, Maybe NL80211Packet) -> Text)
    -> Text
    -> String
    -> IO WifiPollHandle
getWifiHandle' f d n = do
    s <- getSSIDSocket
    i <- fromMaybe (error ("Could not find interface: " ++ n)) <$> getInterface s n
    return (WH s i f d)

-- |Get a wifi handle
getWifiHandle
  :: [WifiFormat] -- ^Format "String" for output generation
  -> Text -- ^Text that should be displayed when wifi is disconnected
  -> String -- ^Name of the interface
  -> IO WifiPollHandle
getWifiHandle f d n =
  getWifiHandle' (getCombiFun f) d n
