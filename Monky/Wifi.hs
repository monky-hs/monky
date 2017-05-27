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
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.Wifi
Description : Gives access to wifi status
Maintainer  : ongy
Stability   : experimental
Portability : Linux

-}
module Monky.Wifi
  ( getCurrentWifi
  , getCurrentWifiStats
  , getInterface
  , gotReadable
  , getSSIDSocket
  , Interface
  , SSIDSocket
  , getWifiFd
  , prepareEvents
  , getExtendedWifi

  , Signal(..)
  , WifiStats(..)
  , WifiConn(..)
  )
where

import Data.Bits ((.&.))
import Data.Word (Word8, Word32)
import Data.Maybe (listToMaybe, fromMaybe)

import System.Posix.Types (Fd)

import System.Linux.Netlink (Packet(..), getAttributes, Attributes)
import System.Linux.Netlink.GeNetlink (GenlHeader(..), GenlData(..))
import System.Linux.Netlink.GeNetlink.NL80211
import System.Linux.Netlink.GeNetlink.NL80211.Constants

import qualified Data.Map as M

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Data.Serialize (Serialize, decode)
import Data.Serialize.Get (runGet, getWord32host)
import Data.Serialize.Put (runPut, putWord32host)

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

-- |The interface identifier
type Interface = Word32

-- |The socket type for this module
-- The first socket is general operation/event based
-- the seconds socket will be used to get extended information.
-- We use 2 sockets, to not get event/request mixed up.
data SSIDSocket = SSIDSocket NL80211Socket NL80211Socket

-- |Enum for connection change
data WifiConn
  = WifiNone -- ^Nothing changed, connection unrelated message
  | WifiDisconnect -- ^The current network was disconnectd
  | WifiConnect WifiStats -- ^A new connection was established

-- |Signal type: http://lxr.free-electrons.com/source/net/wireless/nl80211.c#L6944
data Signal
  = SigMBM Word32 -- ^Signal MBM
  | SigUNSPEC Word8 -- ^Strength 0-100 http://lxr.mein.io/source/iwinfo/api/nl80211.h#L3388

-- |Wifi network connection information
data WifiStats = WifiStats
  { wifiChannel :: Word8
  , wifiRates :: [Word32]
  , wifiName :: String
  , wifiFreq :: Word32
  , wifiSig :: Signal
  , wifiBSSID :: ByteString
  }

-- Unsafe decode, we rely on kernel to be sensible
uDecode :: Serialize a => Maybe ByteString -> Maybe a
uDecode = fmap (\bs -> let (Right x) = decode bs in x)

uGetWord32 :: Maybe ByteString -> Maybe Word32
uGetWord32 = fmap (\bs -> let (Right x) = runGet getWord32host bs in x)

getBssAttrs :: Attributes -> Maybe Attributes
getBssAttrs attr = do
  bs <- M.lookup eNL80211_ATTR_BSS attr
  case runGet getAttributes bs of
    (Left _)  -> Nothing
    (Right x) -> return x

-- |Convert raw values from netlink
getSignal :: Maybe Word32 -> Maybe Word8 -> Signal
getSignal Nothing    (Just unspec) = SigUNSPEC unspec
getSignal (Just mbm) Nothing       = SigMBM mbm
getSignal x          y             = error ("Wifi signal is weird, should be either, got: " ++ show x ++ " and " ++ show y)

-- |Get WifiStats from netlink message
attrToStat :: NL80211Packet -> Maybe WifiStats
attrToStat pack = do
  pattrs <- getBssAttrs $ packetAttributes pack
  attrs <- getWifiAttributes pack

  name <- fmap show . M.lookup eWLAN_EID_SSID $ attrs
  channel <- uDecode . M.lookup eWLAN_EID_DS_PARAMS $ attrs
  rate <- M.lookup eWLAN_EID_SUPP_RATES attrs

  freq <- uDecode . M.lookup eNL80211_BSS_FREQUENCY $ pattrs
  ssid <- M.lookup eNL80211_BSS_BSSID pattrs
  let mbm = uGetWord32 . M.lookup eNL80211_BSS_SIGNAL_MBM $ pattrs
  let sig = uDecode . M.lookup eNL80211_BSS_SIGNAL_UNSPEC $ pattrs

  let bs = M.lookup eWLAN_EID_EXT_SUPP_RATES attrs
  let ratL = rate `BS.append` fromMaybe BS.empty bs
  let rates = map (\y -> fromIntegral (y .&. 0x7F) * (500000 :: Word32)) . BS.unpack $ ratL

  return $ WifiStats channel rates name freq (getSignal mbm sig) ssid

-- |Get the stats of a currently connected wifi network
getCurrentWifiStats :: SSIDSocket -> Interface -> IO (Maybe WifiStats)
getCurrentWifiStats (SSIDSocket _ s) i = do
  wifis <- getConnectedWifi s i
  return $ attrToStat =<< listToMaybe  wifis


-- |Get only the name of the currently connected wifi
getCurrentWifi :: SSIDSocket -> Interface -> IO (Maybe String)
getCurrentWifi s i = fmap wifiName <$> getCurrentWifiStats s i


-- |Get the interface id by name
getInterface :: SSIDSocket -> String -> IO (Maybe Interface)
getInterface (SSIDSocket s _) n = do
  interfaces <- getInterfaceList s
  return $ snd <$> listToMaybe (filter ((==) n . fst) interfaces)


-- |get the raw fd for eventing
getWifiFd :: SSIDSocket -> Fd
getWifiFd (SSIDSocket s _) = getFd s

-- We are only looking for ESSID right now, if we want to
-- make this module more general, we will have to extend the
-- return type of this function
-- |This should be called when the fd returned by 'getWifiFd' got readable
gotReadable :: SSIDSocket -> Interface -> IO WifiConn
gotReadable b@(SSIDSocket s _) i = do
-- we only care for ESSID and connect updates are a single message
-- so this *should* be fine
  ps <- getPacket s
  if null ps
    then error "Failed to get a package in gotReadable, this should not be possible"
    else do
      let packet = head ps
      let cmd = genlCmd . genlDataHeader . packetCustom $ packet
      if cmd == eNL80211_CMD_CONNECT
        then do
          wifi <- getCurrentWifiStats b i
          return $ case wifi of
            Nothing -> WifiDisconnect
            Just x -> WifiConnect x
        else if cmd == eNL80211_CMD_DISCONNECT
          then let bs = M.lookup eNL80211_ATTR_IFINDEX (packetAttributes packet) in
            if maybe False (== i) . uGetWord32 $ bs
              then return WifiDisconnect
              else return WifiNone
          else return WifiNone

getStation :: NL80211Socket -> Word32 -> ByteString -> IO [NL80211Packet]
getStation s i m = query s eNL80211_CMD_GET_STATION False attrs
    where attrs = M.fromList [(eNL80211_ATTR_IFINDEX, runPut . putWord32host $ i), (eNL80211_ATTR_MAC, m)]

-- | Get some additional information about the currently connected wifi
getExtendedWifi :: SSIDSocket -> Interface -> WifiStats -> IO (Maybe NL80211Packet)
getExtendedWifi (SSIDSocket _ s) i stats =
    listToMaybe <$> getStation s i (wifiBSSID stats)

-- |Subscribe to multicast group
prepareEvents :: SSIDSocket -> IO ()
prepareEvents (SSIDSocket s _) = joinMulticastByName s "mlme"

-- |Get a netlink socket bound to nl80211
-- Before this is used event based, call 'prepareEvents'
getSSIDSocket :: IO SSIDSocket
getSSIDSocket = do
  s <- makeNL80211Socket
  e <- makeNL80211Socket
  return $ SSIDSocket s e
