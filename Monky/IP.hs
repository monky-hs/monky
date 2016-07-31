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
{-# LANGUAGE TupleSections #-}
{-|
Module      : Monky.IP
Description : IP information collection module
Maintainer  : ongy
Stability   : experimental
Portability : Linux

Some of this should (and will) move to the netlink package at some point in time.

This mostly exists, because both i3-status and conky have
modules like this.
IPs have become complicated and I don't really know how
to sanely display IP in formation for an interface.
Feel free to create issues/contact me on IRC if you
want to use this and need a better interface (maybe network length information?)
-}
module Monky.IP
  ( IPHandle
  , getSocket
  , getAddresses

  , AddressFamily(..)
  , IP(..)
  , getRawFd
  , subscribeToEvents
  , handleNext
  )
where

import System.IO (hPutStrLn, stderr)
import Data.Bits ((.|.))
import Data.Maybe (mapMaybe)
import Data.Word (Word32)

import System.Posix.Types (Fd)

import System.Linux.Netlink
import System.Linux.Netlink.Constants hiding (AddressFamily)
import System.Linux.Netlink.Route

import qualified Data.ByteString.Char8 as BSC (pack)
import qualified Data.ByteString as BS (singleton, append)

import qualified Data.Map as M

import Monky.IP.Raw

-- |Handle to access information about the IPs on the system
data IPHandle = IPHandle NetlinkSocket AddressFamily Word32

-- This is RTNLGRP_IPV4_IFADDR, which is not currently exported by the netlink library
cRTNLGRP_IPV4_IFADDR :: Num a => a
cRTNLGRP_IPV4_IFADDR = 5

cRTNLGRP_IPV6_IFADDR :: Num a => a
cRTNLGRP_IPV6_IFADDR = 9

linkQuery :: String -> RoutePacket
linkQuery name =
  let flags = fNLM_F_REQUEST -- .|. fNLM_F_MATCH .|. fNLM_F_ROOT
      header = Header eRTM_GETLINK flags 0 0
      msg = NLinkMsg 0 0 0
      attrs = M.fromList [(eIFLA_IFNAME, BSC.pack name `BS.append` BS.singleton 0)]
      in
    Packet header msg attrs


getInterfaceID :: NetlinkSocket -> String -> IO Word32
getInterfaceID sock name = do
  interfaces <- query sock $ linkQuery name
  let ids = map (\(Packet _ (NLinkMsg _ index _) _) -> index) interfaces
  return $ head ids

-- |Get an IPHandle to gather information about IPs on the system.
getSocket
  :: String -- ^The interface to monitor
  -> AddressFamily -- ^The AddressFamily to use
  -> IO IPHandle
getSocket name fam = do
  sock <- makeSocket
  iid <- getInterfaceID sock name
  return $ IPHandle sock fam iid

-- I'm not quite sure why, but apparently RTM_GETADDR only has a version for dumps, not
-- normal requests, so we have to filter the output either way.
addressQuery :: AddressFamily -> RoutePacket
addressQuery fam =
  let flags = fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT
      header = Header eRTM_GETADDR flags 0 0
      msg = NAddrMsg (familyToNum fam) 0 0 0 0
      attrs = M.empty
      in
    Packet header msg attrs

-- |Get all addresses on the system (filtered by interface and type, see 'getSocket')
getAddresses :: IPHandle -> IO [IP]
getAddresses (IPHandle sock fam iid) = do
  packs <- query sock $ addressQuery fam
  let matching =  filter (\(Packet _ (NAddrMsg _ _ _ _ aid) _) -> aid == iid) packs
  let addrs = mapMaybe (M.lookup eIFLA_ADDRESS . packetAttributes) matching
  return $ map (ipFromBS) addrs

-- |Get the raw 'Fd' used by this handle. This can be used to block in RTS controlled manner
getRawFd :: IPHandle -> Fd
getRawFd (IPHandle sock _ _) = getNetlinkFd sock

-- |Subscribe to the multicast group(s) for this handle. This has to be called ONCE before 'handleNext' can be used.
subscribeToEvents :: IPHandle -> IO ()
subscribeToEvents (IPHandle sock fam _) =
  let grps = case fam of
               AF_UNSPEC -> [cRTNLGRP_IPV6_IFADDR, cRTNLGRP_IPV4_IFADDR]
               AF_INET -> [cRTNLGRP_IPV4_IFADDR]
               AF_INET6 -> [cRTNLGRP_IPV6_IFADDR]
               in
    mapM_ (joinMulticastGroup sock) grps


-- Handle -> add -> remove -> done
{- |Handle the next event from Netlink

This will handle the next event reported by the handle.
Before this ever does anything 'subscribeToEvents' has to be called ONCE!
This function can then be called as often as needed and should be called
in a main loop.

The functions passed may be called multiple times, and sometimes both may be called.
This is a limitation by the netlink api, that cannot be avoided without user space buffers.
-}
handleNext
  :: IPHandle -- ^The handle to use
  -> (IP -> IO ()) -- ^Function to call with new ip addresses
  -> (IP -> IO ()) -- ^Function to call with removed addresses
  -> IO ()
handleNext (IPHandle sock _ iid) add remove = do
  evt <- recvOne sock
  let matching = filter (\(Packet _ (NAddrMsg _ _ _ _ aid) _) -> aid == iid) evt
  let addrs = mapMaybe transform matching
  mapM_ handle addrs
  where handle (cmd, addr)
          | cmd == eRTM_NEWADDR = add addr
          | cmd == eRTM_DELADDR = remove addr
          | otherwise = hPutStrLn stderr ("Got unexpeced message while handling IPevents: " ++ show cmd)
        transform :: RoutePacket -> Maybe (MessageType, IP)
        transform (Packet (Header cmd _ _ _) _ attrs) =
          fmap ((cmd,) . ipFromBS) . M.lookup eIFLA_ADDRESS $ attrs
        transform err = error ("Something went wrong while handling IP events: " ++ show err)
