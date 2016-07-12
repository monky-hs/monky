{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}

module Monky.Network.Dynamic
  ( getUHandles
  , UHandles
  , Handles
  , getMultiReadWrite
  )
where

import Data.Bits ((.|.))
import Control.Monad (when)
import Control.Concurrent (forkIO, threadWaitRead)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M
import Data.IORef

import qualified Data.ByteString.Char8 as BS

import System.Linux.Netlink
import System.Linux.Netlink.Constants
import System.Linux.Netlink.Route

import Monky.Network.Static

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

-- |A Wrapper than also carries the name, for comparision
type NetHandle = (String, NetworkHandle)

-- |The map we keep our handles in (the Int is the Interface ID on the system)
type Handles = IntMap NetHandle

-- |The actual handel exposed and used by this module
type UHandles = (IORef Handles, String -> Bool)

instance {-# OVERLAPPING #-} Show NetHandle where
  show (x, _) = x


-- |The fold function used for 'getMultiReadWrite' handling the IO and Maybe stuff
foldF :: NetworkHandle -> IO (Maybe (Int, Int)) -> IO (Maybe (Int, Int))
foldF h o = do
  m <- getReadWrite h
  case m of
    (Just (r, w)) -> do
      om <- o
      case om of
        Just (oldr, oldw) -> return $ Just (oldr + r, oldw + w)
        Nothing -> return $ Just (r, w)
    Nothing -> o


-- |Get the sum of all read/write rates from our network devices or Nothing if none is active
getMultiReadWrite :: Handles -> IO (Maybe (Int, Int))
getMultiReadWrite =
  IM.foldr (\(_, v) -> foldF v) (return Nothing)


-- |Logic for adding a new device to our Handles
gotNew :: Int -> String -> Handles -> IO Handles
gotNew index name m =
  case IM.lookup index m of
    Nothing -> do
      h <- getNetworkHandle name
      return $IM.insert index (name, h) m
    Just (x, v) -> if x == name
      then return m
      else do
        h <- getNetworkHandle name
        closeNetworkHandle v
        return $IM.adjust (const (name, h)) index m


-- |Logic for removing a handle form Handles after we lost the interface
lostOld :: Int -> Handles -> IO Handles
lostOld index m = case IM.lookup index m of
  Nothing -> return m
  (Just (_, h)) ->
    closeNetworkHandle h >>
    return (IM.delete index m)


-- |The packet used to drump all current network devices
requestPacket :: RoutePacket
requestPacket =
  let flags = fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT
      header = Header eRTM_GETLINK flags 42 0
      msg = NLinkMsg 0 0 0 in
    Packet header msg M.empty


-- |Read the interface name and index from 'RoutePacket'
readInterface :: RoutePacket -> (Int, String)
readInterface (Packet _ msg attrs) =
  let (Just name) = M.lookup eIFLA_IFNAME attrs
      names = init $ BS.unpack name -- Drop \0
      index = interfaceIndex msg in
    (fromIntegral index, names)
readInterface x = error ("Something went wrong while getting interfaces: " ++ show x)


-- |Get all current interfaces from our system
getCurrentDevs :: IO [(Int, String)]
getCurrentDevs = do
  sock <- makeSocket
  ifs <- query sock requestPacket
  closeSocket sock
  return $map readInterface ifs


-- |Get the 'Handles' wrapper for all current interfaces
getNetworkHandles :: (String -> Bool) -> IO Handles
getNetworkHandles f = do
  interfaces <- filter (f . snd) <$> getCurrentDevs
  foldr build (return IM.empty) interfaces
  where build (index, dev) m =
          gotNew index dev =<< m


-- |Handle an incomming rtneltink message and update the handle
doUpdate :: UHandles -> RoutePacket -> IO ()
doUpdate (mr, f) (Packet hdr msg attrs)
-- for now we will assume that we want the interface
  | messageType hdr == eRTM_NEWLINK = do
    let (Just name) = M.lookup eIFLA_IFNAME attrs
    let names = init $BS.unpack name -- Drop \0
    when (f names) $ do -- Add
      let index = interfaceIndex msg
      m <- readIORef mr
      nm <- gotNew (fromIntegral index) names m
      writeIORef mr nm
  | messageType hdr == eRTM_DELLINK = do
    let index = interfaceIndex msg
    m <- readIORef mr
    nm <- lostOld (fromIntegral index) m
    writeIORef mr nm
  | otherwise = return ()
-- Ignore everything else
doUpdate _ _ = return ()


-- |Updater loop, it blocks on the netlink socker until it gets a message
updaterLoop :: NetlinkSocket -> UHandles -> IO ()
updaterLoop sock h = do
  threadWaitRead (getNetlinkFd sock)
  packet <- recvOne sock :: IO [RoutePacket]
  mapM_ (doUpdate h) packet
  updaterLoop sock h


-- |Start the update loop for adding/removing interfaces
updater :: UHandles -> IO ()
updater h = do
  sock <- makeSocket
  joinMulticastGroup sock 1
  updaterLoop sock h


-- |Get the new handle this module exports and start its updater loop
getUHandles
  :: (String -> Bool) -- ^Will be given the name of the handle, only add interface if this returns true (think of it as filter over all possible interfaces)
  -> IO UHandles
getUHandles f = do
  handle <- getNetworkHandles f
  ref <- newIORef handle
  _ <- forkIO (updater (ref, f))
  return (ref, f)
