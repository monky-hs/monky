module Monky.Wifi
where

import System.Linux.Netlink.GeNetlink.NL80211
--import System.Linux.Netlink.GeNetlink.NL80211.Constants

import Data.Word (Word32)
import Data.Maybe

data WifiHandle = WifiHandle NL80211Socket Word32

getWifiHandle :: String -> IO WifiHandle
getWifiHandle iface = do
  sock <- makeNL80211Socket
  interfaces <- getInterfaceList sock
  let [(name, iid)] = filter ((== iface) . fst) interfaces
  return $WifiHandle sock iid

getCurrentWifiName :: WifiHandle -> IO (Maybe String)
getCurrentWifiName (WifiHandle sock iface) = do
  connected <- getConnectedWifi sock iface
  let attrs = mapMaybe getWifiAttributes connected
  putStrLn $show attrs
  return Nothing
