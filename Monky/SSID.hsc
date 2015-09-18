{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : SSID
Description : Provides an interface to the ssid a network device is connected to
Maintainer  : ongy
Stability   : experimental
Portability : Linux

This does sometimes fail because the ioctl fails.
When this fails is nondeterministic.
This module will soon be replaced by netlink based module.
This module treats the SSID as C-string, which is wrong but this will not be
fixed!! The netlink implementation will replace this.
-}
module Monky.SSID
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

-- |The handle exported by this module
data SSIDHandle = SSIDH SSIDStruct

data IWPoint a = IWPoint (Ptr a) Word16 Word16

data SSIDStruct = SSIDStruct [CChar] (IWPoint CChar)

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


{-| Get the ssid of the network the device is conncted to or an empty string if
it isn't connected
-}
getCurrentSSID :: SSIDHandle -> IO String
getCurrentSSID (SSIDH s) = getSSID s

-- |Get the handle for this module
getSSIDHandle :: String -> IO SSIDHandle
getSSIDHandle dev = liftM SSIDH $getSSIDStruct 128 dev
