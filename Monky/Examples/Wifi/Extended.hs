{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumDecimals #-}
module Monky.Examples.Wifi.Extended
where

import Formatting
import Data.Text (Text)
import qualified Data.Text as T
import Monky.Examples.Wifi
import Monky.Examples.Utility
import System.Linux.Netlink.GeNetlink.NL80211
import System.Linux.Netlink.GeNetlink.NL80211.StaInfo

import Control.Applicative ((<|>), (<$>), (<*>))


data ExtWifiFormat
    -- |The MCSIndex for our connection
    = MCSIndex
    -- |The Signal width (in MHz)
    | WifiWidth
    -- |The TX Bitrate of our station
    | Bitrate
    -- |Minimum of TX/RX Bitrate for this station
    | BitrateMin
    -- |Signal strength from other source
    | ExtSignal
    -- |Signal strength average
    | ExtSignalAverage
    -- |Pure text for formatting
    | ExtText Text

getExtFun :: ExtWifiFormat -> StaInfo -> Text
getExtFun MCSIndex info = -- MCS is in the TXRate
    case staTXRate info of
        Nothing -> "No Rate"
        Just x -> case rateMCS x <|> rateVHTMCS x of
            Nothing -> "No MCS"
            Just y -> sformat int y
getExtFun WifiWidth info = -- Width is also in TXRate
    case staTXRate info of
        Nothing -> "No Rate"
        Just x -> case rateWidthFlag x of
            Width5MHz -> "5MHz"
            Width10MHz -> "10MHz"
            Width20MHz -> "20MHz"
            Width40MHz -> "40MHz"
            Width80MHz -> "80MHz"
            Width160MHz -> "160MHz"
getExtFun Bitrate info = -- Bitrate is in TXRate
    case staTXRate info of
        Nothing -> "No Rate"
        Just x -> maybe
            "No Bitrate"
            (flip convertUnitSI "b" . (* 1e5))
            (rateBitrate x)
getExtFun BitrateMin info = -- Bitrate from RX/TX Rate
    let tx = rateBitrate =<< staTXRate info
        rx = rateBitrate =<< staRXRate info
    in case min <$> rx <*> tx of
        Nothing -> "No rates"
        Just x -> convertUnitSI (x * 1e5) "b"
getExtFun ExtSignal info =
    case staSignalMBM info of
        Nothing -> "No strength"
        Just x -> sformat int . doMBM $ fromIntegral x
getExtFun ExtSignalAverage info =
    case staSignalMBMA info of
        Nothing -> "No strength"
        Just x -> sformat int . doMBM $ fromIntegral x
getExtFun (ExtText x) _ = x

getExtFunction :: [ExtWifiFormat] -> StaInfo -> Text
getExtFunction xs = T.concat . (sequence . map getExtFun $ xs)

{- |This function is the easiest, but also a bit limited way to get extended wifi information.

With this, all "normal" information will be first, and the extended information will be appended.
-}
getCombiFun :: [WifiFormat] -> [ExtWifiFormat] -> ((WifiStats, Maybe NL80211Packet) -> Text)
getCombiFun xs ys (stat, ext) =
    let old = getFunction xs stat
        info = staInfoFromPacket =<< ext
        new = case info of
                Just x -> getExtFunction ys x
                Nothing -> "None"
    in old `T.append` new
