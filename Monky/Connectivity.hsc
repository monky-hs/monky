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
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Monky.Connectivity
Description : Allows the user to get a connected state
Maintainer  : ongy
Stability   : experimental
Portability : Linux

This module checks periodically if the current system
can establish a network connection (TCP) to a given host
on a given port. It does not care about reject, it is
intended to test whether a firewall drops packages or
a (tethered) connection is stable.
-}
module Monky.Connectivity
  ( ConnHandle
  , getConnH
  , hasConn
  )
where

import Data.Bits ((.|.))
import Control.Concurrent (threadWaitWrite, threadDelay, forkIO)
import Data.Word (Word16)
import Foreign.C.Error (getErrno, Errno(..), eINPROGRESS)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt(..), CLong(..))
import Foreign.Storable (Storable(..))
import System.Posix.Types (Fd(..))
import System.Timeout (timeout)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)

import Monky.IP.Raw



#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/ip.h>

-- |The Haskell type for the C struct sockaddr
newtype Port = Port Word16 deriving (Eq, Show)
data Sockaddr = Socka Int Port IP4 deriving (Eq, Show)

-- |Raw socket calls, we node those
foreign import ccall "socket" c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "close" c_close :: CInt -> IO ()
foreign import ccall "connect" c_connect :: CInt -> Ptr Sockaddr -> CInt -> IO CInt

-- This isn't really IO, since it is deterministic and doesn't have sideeffects
foreign import ccall "htons" htons :: Word16 -> Word16


instance Storable Sockaddr where
  sizeOf _ = #{size struct sockaddr_in}
  alignment _ = alignment (undefined :: CLong)
  peek p = do
    fam <- #{peek struct sockaddr_in, sin_family} p
    port <- #{peek struct sockaddr_in, sin_port} p
    ip <- #{peek struct sockaddr_in, sin_addr} p
    return (Socka fam (Port port) ip)
  poke p (Socka fam (Port port) ip) = do
    #{poke struct sockaddr_in, sin_family} p fam
    #{poke struct sockaddr_in, sin_port} p port
    #{poke struct sockaddr_in, sin_addr} p ip


-- TODO maybe create an echo service so we don't have to do the socket call all the time
tryConn :: String -> Int -> IO Bool
tryConn ip port = do
  socket <- c_socket #{const AF_INET} (#{const SOCK_STREAM} .|. #{const SOCK_NONBLOCK}) 0
  -- This will always be -1 because of how unblocking sockets work
  _ <- with (Socka #{const AF_INET} (Port . htons $fromIntegral port) (parseIP ip))
       (\ptr ->c_connect socket ptr (fromIntegral $sizeOf (undefined :: Sockaddr)))
  (Errno con) <- getErrno
  ret <- if (Errno con) == eINPROGRESS
    then timeout (500 * 1000) (threadWaitWrite (Fd socket)) >>=
      \case
        Nothing -> return True
        Just _ -> return False
    else return False
  c_close socket
  return ret

-- |The handle exposed by this module
data ConnHandle = ConnH String Int (IORef Bool)


-- |Get the current connected state from the handle
hasConn :: ConnHandle -> IO Bool
hasConn (ConnH _ _ r) = readIORef r


updateLoop :: ConnHandle -> IO ()
updateLoop h@(ConnH ip port ref) = do
  writeIORef ref =<< tryConn ip port
  -- Sleep 1 second
  threadDelay (1000*1000)
  updateLoop h

-- |Get a handle to check for connectivity
getConnH
  :: String -- ^The Host to use for connectivity probing
  -> Int -- ^Which port to use for connecivity probing (drop is bad)
  -> IO ConnHandle
getConnH ip port = do
  ref <- newIORef False
  let h = ConnH ip port ref
  _ <- forkIO (updateLoop h)
  return h


