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
{-|
Module      : Monky.IP.Raw
Description : Lowlevel IP interfaces
Maintainer  : ongy
Stability   : experimental
Portability : Linux

This may change at any time when the main IP module changes.
Consider this API unstable!
-}
module Monky.IP.Raw
  ( IP(..)
  , IP4
  , IP6

  , parseIP
  , ipFromBS
  , familyToNum
  , AddressFamily(..)
  , getAddrFamily
  )
where

import Data.ByteString (ByteString, useAsCStringLen, packCStringLen)
import qualified Data.ByteString as BS (length)
import Data.Serialize (decode)

import Data.Word (Word32, Word64)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt(..), CChar)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable(..))

import System.IO.Unsafe (unsafePerformIO)

foreign import ccall "inet_pton" c_pton :: CInt -> CString -> Ptr IP4 -> IO ()
foreign import ccall "inet_ntop" c_ntop :: CInt -> Ptr a -> Ptr CChar -> Word64 -> IO (Ptr CChar)
foreign import ccall "ntohl" ntohl :: Word32 -> Word32
foreign import ccall "htonl" htonl :: Word32 -> Word32

foreign import ccall "memcpy" memcpy :: Ptr a -> Ptr b -> Word64 -> IO ()

-- |IPv4 addresses
newtype IP4 = IP4 Word32 deriving (Eq)
-- |IPv6 addresses
newtype IP6 = IP6 ByteString deriving (Eq)

-- |Datatype for IP addresses, abstracts over v4/v6
data IP
  = IPv4 IP4
  | IPv6 IP6
  deriving (Eq)

-- |AddressFamilies support for libraries
data AddressFamily
  = AF_UNSPEC
  | AF_INET
  | AF_INET6



#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/ip.h>
instance Storable IP4 where
  sizeOf _ = 4
  alignment _ = alignment (undefined :: Word32)
  -- be compatible with the default implementation hsc chooses
  peek p = fmap (IP4 . ntohl) . peek $ castPtr p
  poke p (IP4 w) = poke (castPtr p) $ htonl w

instance Storable IP6 where
  sizeOf _ = 16
  alignment _ = alignment (undefined :: Word64)
  peek p = fmap IP6 $ packCStringLen (castPtr p, 16)
  poke p (IP6 w) = useAsCStringLen w (\(b, _) -> memcpy p b 16)

instance Show IP where
  show (IPv4 ip) = show ip
  show (IPv6 ip) = show ip


instance Show IP6 where
  show = showIP6

instance Show IP4 where
  show = showIP


showIPIO :: IP4 -> IO String
showIPIO ip = allocaBytes #{const INET_ADDRSTRLEN} (\str ->
  with ip (\ptr -> c_ntop (familyToNum AF_INET) ptr str #{const INET_ADDRSTRLEN}) >> peekCString str)

-- All sideeffects are contained in the IO action and it is deterministic, so we can drop the IO
showIP :: IP4 -> String
showIP ip = unsafePerformIO (showIPIO ip)
{-# NOINLINE showIP #-}


parseIPIO :: String -> IO IP4
parseIPIO xs =
  withCString xs (\str -> do
    alloca (\ptr -> c_pton (familyToNum AF_INET) str ptr >> peek ptr))

-- All sideeffects are contained in the IO action and it is deterministic, so we can drop the IO
-- |Parse an IP4 from a String
parseIP :: String -> IP4
parseIP str = unsafePerformIO (parseIPIO str)
{-# NOINLINE parseIP #-}


-- |Read an IP from a ByteString. The type is determined by the size of the ByteString.
ipFromBS :: ByteString -> IP
ipFromBS bs = if BS.length bs == 16
  then IPv6 (IP6 bs)
  else case decode bs of
    (Left err) -> error ("Failed to decode ip: " ++ err)
    (Right x)  -> IPv4 (IP4 x)


showIP6IO :: IP6 -> IO String
showIP6IO ip = allocaBytes #{const INET_ADDRSTRLEN} (\str ->
  with ip (\ptr -> c_ntop (familyToNum AF_INET6) ptr str #{const INET6_ADDRSTRLEN}) >> peekCString str)

-- All sideeffects are contained in the IO action and it is deterministic, so we can drop the IO
showIP6 :: IP6 -> String
showIP6 ip = unsafePerformIO (showIP6IO ip)
{-# NOINLINE showIP6 #-}


-- A few things good to have:
-- |Get the number associated with the family address. This is for interfacing with libraries
familyToNum :: Num a => AddressFamily -> a
familyToNum AF_UNSPEC = 0
familyToNum AF_INET = #{const AF_INET}
familyToNum AF_INET6 = #{const AF_INET6}

-- |Get the address family for a given ip address
getAddrFamily :: IP -> AddressFamily
getAddrFamily (IPv6 _) = AF_INET6
getAddrFamily (IPv4 _) = AF_INET
