{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
module Monky.Connectivity
  ( ConnHandle(..)
  , getConnH
  )
where

import Data.Bits ((.|.))
import Control.Concurrent (threadWaitWrite, threadDelay, forkIO)
import Data.Word (Word64,Word32,Word16)
import Foreign.C.Error (getErrno, Errno(..), eINPROGRESS)
import Foreign.C.Types (CInt(..), CLong(..))
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable(..))
import System.Posix.Types (Fd(..))
import System.Timeout (timeout)
import Data.IORef (IORef, newIORef, writeIORef)


import System.IO.Unsafe (unsafePerformIO)

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/ip.h>

-- |The Haskell type for the C struct sockaddr
newtype Port = Port Word16 deriving (Eq, Show)
newtype IP4 = IP4 Word32 deriving (Eq)
data Sockaddr = Socka Int Port IP4 deriving (Eq, Show)

-- |Raw socket calls, we node those
foreign import ccall "socket" c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall "close" c_close :: CInt -> IO ()
foreign import ccall "connect" c_connect :: CInt -> Ptr Sockaddr -> CInt -> IO CInt

foreign import ccall "inet_pton" c_pton :: CInt -> CString -> Ptr IP4 -> IO ()
foreign import ccall "inet_ntop" c_ntop :: CInt -> Ptr IP4 -> Ptr CChar -> Word64 -> IO (Ptr CChar)
-- This isn't really IO, since it is deterministic and doesn't have sideeffects
foreign import ccall "htons" htons :: Word16 -> Word16
foreign import ccall "htonl" htonl :: Word32 -> Word32
foreign import ccall "ntohl" ntohl :: Word32 -> Word32


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


instance Storable IP4 where
  sizeOf _ = 4
  alignment _ = alignment (undefined :: Word32)
  -- be compatible with the default implementation hsc chooses
  peek p = fmap (IP4 . ntohl) . peek $ castPtr p
  poke p (IP4 w) = poke (castPtr p) $ htonl w



instance Show IP4 where
  show = showIP

showIPIO :: IP4 -> IO String
showIPIO ip = allocaBytes #{const INET_ADDRSTRLEN} (\str ->
  with ip (\ptr -> c_ntop #{const AF_INET} ptr str #{const INET_ADDRSTRLEN}) >> peekCString str)

-- All sideeffects are contained in the IO action and it is deterministic, so we can drop the IO
showIP :: IP4 -> String
showIP ip = unsafePerformIO (showIPIO ip)


parseIPIO :: String -> IO IP4
parseIPIO xs =
  withCString xs (\str -> do
    alloca (\ptr -> c_pton #{const AF_INET} str ptr >> peek ptr))

-- All sideeffects are contained in the IO action and it is deterministic, so we can drop the IO
parseIP :: String -> IP4
parseIP str = unsafePerformIO (parseIPIO str)


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


