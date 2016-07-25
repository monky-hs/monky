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

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

-- |The interface identifier
type Interface = Word32
-- |The socket type for this module
type SSIDSocket = NL80211Socket

-- |Enum for connection change
data WifiConn
  = WifiNone -- ^Nothing changed, connection unrelated message
  | WifiDisconnect -- ^The current network was disconnectd
  | WifiConnect WifiStats -- ^A new connection was established

-- |Wifi network connection information
data WifiStats = WifiStats
  { wifiChannel :: Word8
  , wifiRates :: [Word32]
  , wifiName :: String
  , wifiFreq :: Word32
  , wifiMBM :: Word32
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


attrToStat :: NL80211Packet -> Maybe WifiStats
attrToStat pack = do
  pattrs <- getBssAttrs $ packetAttributes pack
  attrs <- getWifiAttributes pack

  name <- fmap show . M.lookup eWLAN_EID_SSID $ attrs
  channel <- uDecode . M.lookup eWLAN_EID_DS_PARAMS $ attrs
  rate <- M.lookup eWLAN_EID_SUPP_RATES attrs

  freq <- uDecode . M.lookup eNL80211_BSS_FREQUENCY $ pattrs
  mbm <- uDecode . M.lookup eNL80211_BSS_SIGNAL_MBM $ pattrs

  let bs = M.lookup eWLAN_EID_EXT_SUPP_RATES attrs
  let ratL = rate `BS.append` fromMaybe BS.empty bs
  let rates = map (\y -> fromIntegral (y .&. 0x7F) * (500000 :: Word32)) . BS.unpack $ ratL

  return $ WifiStats channel rates name freq mbm

-- |Get the stats of a currently connected wifi network
getCurrentWifiStats :: SSIDSocket -> Interface -> IO (Maybe WifiStats)
getCurrentWifiStats s i = do
  wifis <- getConnectedWifi s i
  return $ attrToStat =<< listToMaybe wifis


-- |Get only the name of the currently connected wifi
getCurrentWifi :: SSIDSocket -> Interface -> IO (Maybe String)
getCurrentWifi s i = fmap wifiName <$> getCurrentWifiStats s i


-- |Get the interface id by name
getInterface :: SSIDSocket -> String -> IO (Maybe Interface)
getInterface s n = do
  interfaces <- getInterfaceList s
  return $ snd <$> listToMaybe (filter ((==) n . fst) interfaces)


-- |get the raw fd for eventing
getWifiFd :: SSIDSocket -> Fd
getWifiFd = getFd

-- We are only looking for ESSID right now, if we want to
-- make this module more general, we will have to extend the
-- return type of this function
-- |This should be called when the fd returned by 'getWifiFd' got readable
gotReadable :: SSIDSocket -> Interface -> IO WifiConn
gotReadable s i = do
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
          wifi <- getCurrentWifiStats s i
          return $ case wifi of
            Nothing -> WifiDisconnect
            Just x -> WifiConnect x
        else if cmd == eNL80211_CMD_DISCONNECT
          then let bs = M.lookup eNL80211_ATTR_IFINDEX (packetAttributes packet) in
            if maybe False (== i) . uGetWord32 $ bs
              then return WifiDisconnect
              else return WifiNone
          else return WifiNone


-- |Get a netlink socket bound to nl80211 mlme Group
getSSIDSocket :: IO SSIDSocket
getSSIDSocket = do
  s <- makeNL80211Socket
  joinMulticastByName s "mlme"
  return s
