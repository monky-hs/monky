{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SSID
(SSIDHandle, getSSIDHandle, getCurrentSSID)
where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad (liftM)
import Network.Socket
import System.Posix.IOCtl
import System.Posix.Types

#include <sys/socket.h>
#include <sys/types.h>
#include <linux/wireless.h>

data SSIDHandle = SSIDH SSIDStruct

data IWPoint a = IWPoint { pointer :: Ptr a, length :: Word16, flags :: Word16 }

data SSIDStruct = SSIDStruct { name :: [CChar], point :: IWPoint CChar }

data SSIDREQ = SSIDREQ

instance Storable (IWPoint a) where
  sizeOf _ = #{size struct iw_point}
  alignment _ = alignment (undefined :: CInt)
  peek p = do
    ptr <- #{peek struct iw_point, pointer} p
    len <- #{peek struct iw_point, length} p
    fla <- #{peek struct iw_point, flags} p
    return (IWPoint ptr len fla)
  poke p (IWPoint ptr len fla) = do
    #{poke struct iw_point, pointer} p ptr
    #{poke struct iw_point, length} p len
    #{poke struct iw_point, flags} p fla

instance Storable SSIDStruct where
  sizeOf _ = #{size struct iwreq}
  alignment _ = alignment (undefined :: CInt)
  peek p = do
    dev <- peekArray0 0 (#{ptr struct iwreq, ifr_name} p)
    poi <- peek (#{ptr struct iwreq, u} p :: Ptr (IWPoint CChar))
    return (SSIDStruct dev poi)
  poke p (SSIDStruct dev poi) = do
    pokeArray0 0 (#{ptr struct iwreq, ifr_name} p) dev
    poke (#{ptr struct iwreq, u} p :: Ptr (IWPoint CChar)) poi

instance IOControl SSIDREQ SSIDStruct where
  ioctlReq _ = #const SIOCGIWESSID


getSSIDStruct :: Int -> String -> IO SSIDStruct
getSSIDStruct size dev = liftM c n
  where
    c x = SSIDStruct (map castCharToCChar dev) (IWPoint x (fromIntegral size) 0)
    n = mallocArray size :: IO (Ptr CChar)

clearSSID :: SSIDStruct -> IO ()
clearSSID (SSIDStruct _ (IWPoint ptr s _)) =
  pokeArray0 0 ptr (replicate (fromIntegral s) 0)

getSSID_ :: SSIDStruct -> IO String
getSSID_ (SSIDStruct _ (IWPoint ptr _ _)) = do
  arr <- peekArray0 0 ptr
  return $map castCCharToChar arr

getSSID :: SSIDStruct -> IO String
getSSID s = do
  fd <- liftM fdSocket $ socket AF_INET Datagram 0
  clearSSID s
  ioctl_ (Fd fd) SSIDREQ s
  getSSID_ s


getCurrentSSID :: SSIDHandle -> IO String
getCurrentSSID (SSIDH s) = getSSID s

getSSIDHandle :: String -> IO SSIDHandle
getSSIDHandle dev = liftM SSIDH $getSSIDStruct 128 dev
