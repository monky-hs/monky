{-# LANGUAGE CPP #-}
module Monky.Wifi
  ( getCurrentWifi
  , getCurrentWifiStats
  , getInterface
  , gotReadable
  , gotReadable'
  , getSSIDSocket
  , Interface
  , SSIDSocket
  , getWifiFd

  , WifiStats(..)
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
import Data.Serialize.Get (runGet)

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

type Interface = Word32
type SSIDSocket = NL80211Socket

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
  rate <- M.lookup eWLAN_EID_SUPP_RATES $ attrs

  freq <- uDecode . M.lookup eNL80211_BSS_FREQUENCY $ pattrs
  mbm <- uDecode . M.lookup eNL80211_BSS_SIGNAL_MBM $ pattrs

  let bs = M.lookup eWLAN_EID_EXT_SUPP_RATES $ attrs
  let ratL = rate `BS.append` fromMaybe (BS.empty) bs
  let rates = map (\y -> fromIntegral (y .&. 0x7F) * (500000 :: Word32)) . BS.unpack $ ratL

  return $ WifiStats channel rates name freq mbm


getCurrentWifiStats :: SSIDSocket -> Interface -> IO (Maybe WifiStats)
getCurrentWifiStats s i = do
  wifis <- getConnectedWifi s i
  return $ attrToStat =<< (listToMaybe wifis)


getCurrentWifi :: SSIDSocket -> Interface -> IO (Maybe String)
getCurrentWifi s i = fmap wifiName <$> getCurrentWifiStats s i


getInterface :: SSIDSocket -> String -> IO (Maybe Interface)
getInterface s n = do
  interfaces <- getInterfaceList s
  return $ snd <$> listToMaybe (filter ((==) n . fst) interfaces)

getWifiFd :: SSIDSocket -> Fd
getWifiFd = getFd

-- We are only looking for ESSID right now, if we want to
-- make this module more general, we will have to extend the
-- return type of this function
gotReadable' :: SSIDSocket -> Interface -> IO (Maybe WifiStats)
gotReadable' s i = do
-- we only care for ESSID and connect updates are a single message
-- so this *should* be fine
  packet <- head <$> getPacket s
  let cmd = genlCmd . genlDataHeader . packetCustom $packet
  if cmd == eNL80211_CMD_CONNECT
    then getCurrentWifiStats s i
    else if cmd == eNL80211_CMD_DISCONNECT
      then return . Just $ WifiStats 0 [] "" 0 0
      else return Nothing

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
