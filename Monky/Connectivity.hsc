{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
module Monky.Connectivity
  ( ConnHandle(..)
  , getConnH
  )
where

import Data.Bits ((.|.))
import Data.List.Split (splitOn)
import Control.Concurrent (threadWaitWrite, threadDelay, forkIO)
import Data.Word (Word32,Word16)
import Foreign.C.Error (getErrno, Errno(..), eINPROGRESS)
import Foreign.C.Types (CInt(..), CLong(..))
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import System.Posix.Types (Fd(..))
import System.Timeout (timeout)
import Data.IORef (IORef, newIORef, writeIORef)

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/ip.h>

-- |The Haskell type for the C struct sockaddr
newtype Port = Port Word16 deriving (Eq, Show)
newtype IP4 = IP4 Word32 deriving (Eq, Show)
data Sockaddr = Socka Int Port IP4 deriving (Eq, Show)

-- |Raw socket calls, we node those
foreign import ccall "socket" c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "close" c_close :: CInt -> IO ()
foreign import ccall "connect" c_connect :: CInt -> Ptr Sockaddr -> CInt -> IO CInt
-- This isn't really IO, since it is deterministic and doesn't have sideeffects
foreign import ccall "htons" htons :: Word16 -> Word16
foreign import ccall "htonl" htonl :: Word32 -> Word32

instance Storable Sockaddr where
  sizeOf _ = #{size struct sockaddr_in}
  alignment _ = alignment (undefined :: CLong)
  peek p = do
    fam <- #{peek struct sockaddr_in, sin_family} p
    port <- #{peek struct sockaddr_in, sin_port} p
    ip <- #{peek struct sockaddr_in, sin_addr} p
    return (Socka fam (Port port) (IP4 ip))
  poke p (Socka fam (Port port) (IP4 ip)) = do
    #{poke struct sockaddr_in, sin_family} p fam
    #{poke struct sockaddr_in, sin_port} p port
    #{poke struct sockaddr_in, sin_addr} p ip

parseIP :: String -> IP4
parseIP xs = let
  -- Read stinrg to numbers
  -- Build IP in host byte order
    [a,b,c,d] = map read $ splitOn "." xs
    ip = a * 16777216 + b * 65536 + c * 256 + d * 1 in
  IP4 $htonl ip

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


data ConnHandle = ConnH String Int (IORef Bool)

updateLoop :: ConnHandle -> IO ()
updateLoop h@(ConnH ip port ref) = do
  writeIORef ref =<< tryConn ip port
  -- Sleep 1 second
  threadDelay (1000*1000)
  updateLoop h

getConnH :: String -> Int -> IO ConnHandle
getConnH ip port = do
  ref <- newIORef False
  let h = ConnH ip port ref
  _ <- forkIO (updateLoop h)
  return h


