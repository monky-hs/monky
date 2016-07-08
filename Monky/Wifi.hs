{-# LANGUAGE CPP #-}
module Monky.Wifi
  ( getCurrentWifi
  , getInterface
  , gotReadable
  , getSSIDSocket
  , Interface
  , SSIDSocket
  , getWifiFd
  )
where

import Data.Word (Word32)
import Data.Maybe (mapMaybe, listToMaybe)

import Control.Monad (join)
import System.Posix.Types (Fd)

import System.Linux.Netlink (Packet(..))
import System.Linux.Netlink.GeNetlink (GenlHeader(..), GenlData(..))
import System.Linux.Netlink.GeNetlink.NL80211
import System.Linux.Netlink.GeNetlink.NL80211.Constants

import qualified Data.Map as M

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

type Interface = Word32
type SSIDSocket = NL80211Socket

getCurrentWifi :: SSIDSocket -> Interface -> IO (Maybe String)
getCurrentWifi s i = do
  eids <- mapMaybe getWifiAttributes <$> getConnectedWifi s i
  return $fmap show .  join $M.lookup 0 <$> listToMaybe eids


getInterface :: SSIDSocket -> String -> IO (Maybe Interface)
getInterface s n = do
  interfaces <- getInterfaceList s
  return $snd <$> listToMaybe (filter ((==) n . fst) interfaces)

getWifiFd :: SSIDSocket -> Fd
getWifiFd = getFd

-- We are only looking for ESSID right now, if we want to
-- make this module more general, we will have to extend the
-- return type of this function
gotReadable :: SSIDSocket -> Interface -> IO (Maybe String)
gotReadable s i = do
-- we only care for ESSID and connect updates are a single message
-- so this *should* be fine
  packet <- head <$> getPacket s
  let cmd = genlCmd . genlDataHeader . packetCustom $packet
  if cmd == eNL80211_CMD_CONNECT
    then getCurrentWifi s i
    else if cmd == eNL80211_CMD_DISCONNECT
      then return (Just "")
      else return Nothing

getSSIDSocket :: IO SSIDSocket
getSSIDSocket = do
  s <- makeNL80211Socket
  joinMulticastByName s "mlme"
  return s
