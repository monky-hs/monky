{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Monky.NewNetwork
  ( getUHandles
  , UHandles
  , Handles
  , getMultiReadWrite
  )
where

import Data.Bits ((.|.))
import System.IO.Error (catchIOError)
import Control.Concurrent (forkIO, threadWaitRead)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M
import Data.Time.Clock.POSIX
import Monky.Utility
import Data.IORef

import qualified Data.ByteString.Char8 as BS

import System.Linux.Netlink
import System.Linux.Netlink.Constants
import System.Linux.Netlink.Route

-- |Current state of network device
data NetState
  = Down -- ^It is down, consider off
  | Up -- ^It is up, consider on
  | Unknown -- ^Unknown, kernel docu says to consider on
  | Dormant -- ^Dormant, consider off

-- |The normal network handle as in Network
data NetworkHandle = NetH File File File (IORef Int) (IORef Int) (IORef POSIXTime)

-- |A Wrapper than also carries the name, for comparision
type NetHandle = (String, NetworkHandle)

-- |The map we keep our handles in (the Int is the Interface ID on the system)
type Handles = IntMap NetHandle

-- |The actual handel exposed and used by this module
type UHandles = (IORef Handles, (String -> Bool))

basePath :: String
basePath = "/sys/class/net/"

readPath :: String
readPath = "/statistics/rx_bytes"

writePath :: String
writePath = "/statistics/tx_bytes"

statePath :: String
statePath = "/operstate"

instance {-# OVERLAPPING #-} Show NetHandle where
  show (x, _) = x

-- |Get the read and write rate from a single network interface
getReadWrite :: NetworkHandle -> IO (Int, Int)
getReadWrite (NetH readf writef _ readr writer timer) = do
  nread <- readValue readf
  nwrite <- readValue writef
  ntime <- getPOSIXTime
  oread <- readIORef readr
  owrite <- readIORef writer
  otime <- readIORef timer
  let cread = oread - nread
  let cwrite = owrite - nwrite
  let ctime = otime - ntime
  writeIORef readr nread
  writeIORef writer nwrite
  writeIORef timer ntime
  return ((cread  * 8) `sdiv` round ctime,
          (cwrite * 8) `sdiv` round ctime)
  where sdiv x 0 = x
        sdiv x y = x `div` y


-- |Get the current network adapter state from kernel
getState :: NetworkHandle -> IO NetState
getState (NetH _ _ statef _ _ _) = do
-- the read can thro an exception if the interace disapperad, we just consider it down
  state <- catchIOError (readLine statef) (\_ -> return "down")
  return $ case state of
    "up" -> Up
    "down" -> Down
    "unknown" -> Unknown
    "dormant" -> Dormant
    _ -> error ("Don't know the network state \"" ++ state ++ "\" yet")

-- 'getReadWrite' but only if the current State of the interface says it's sensible
getReadWriteM :: NetworkHandle -> IO (Maybe (Int, Int))
getReadWriteM h = do
  state <- getState h
  case state of
    Up -> Just <$> getReadWrite h
    Down -> return Nothing
    Unknown -> Just <$> getReadWrite h
    Dormant -> return Nothing


-- |The fold function used for 'getMultiReadWrite' handling the IO and Maybe stuff
foldF :: NetworkHandle -> IO (Maybe (Int, Int)) -> IO (Maybe (Int, Int))
foldF h o = do
  m <- getReadWriteM h
  case m of
    (Just (r, w)) -> do
      om <- o
      case om of
        Just (oldr, oldw) -> return $ Just (oldr + r, oldw + w)
        Nothing -> return $ Just (r, w)
    Nothing -> o


-- |Get the sum of all read/write rates from our network devices or Nothing if none is active
getMultiReadWrite :: Handles -> IO (Maybe (Int, Int))
getMultiReadWrite h = do
  foldr (\(_, v) -> foldF v) (return start) h
  where start = Just (0, 0)


-- |Get a network handle (for a single device)
getNetworkHandle :: String -> IO NetworkHandle
getNetworkHandle dev = do
  let path = basePath ++ dev
  readf <- fopen $ path ++ readPath
  writef <- fopen $ path ++ writePath
  statef <- fopen $ path ++ statePath
  readr <- newIORef (1 :: Int)
  writer <- newIORef (1 :: Int)
  timer <- newIORef (0 :: POSIXTime)
  return $NetH readf writef statef readr writer timer


-- |Close a network handle after it is no longer needed (the device disappeared)
closeNetworkHandle :: NetworkHandle -> IO ()
closeNetworkHandle (NetH readf writef statef _ _ _) =
  fclose readf >> fclose writef >> fclose statef


-- |Logic for adding a new device to our Handles
gotNew :: Int -> String -> Handles -> IO Handles
gotNew index name m = do
  case IM.lookup index m of
    Nothing -> do
      h <- getNetworkHandle name
      return $IM.insert index (name, h) m
    Just (x, v) -> if x == name
      then return m
      else do
        h <- getNetworkHandle name
        closeNetworkHandle v
        return $IM.adjust (\_ -> (name, h)) index m


-- |Logic for removing a handle form Handles after we lost the interface
lostOld :: Int -> Handles -> IO Handles
lostOld index m = case IM.lookup index m of
  Nothing -> return m
  (Just (_, h)) ->
    closeNetworkHandle h >>
    return (IM.delete index m)


requestPacket :: RoutePacket
requestPacket =
  let flags = fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT
      header = Header eRTM_GETLINK flags 42 0
      msg = NLinkMsg 0 0 0 in
    Packet header msg M.empty


readInterface :: RoutePacket -> (Int, String)
readInterface (Packet _ msg attrs) =
  let (Just name) = M.lookup eIFLA_IFNAME attrs
      names = init $BS.unpack name
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


getNetworkHandles :: (String -> Bool) -> IO Handles
getNetworkHandles f = do
  interfaces <- filter (f . snd) <$> getCurrentDevs
  foldr build (return IM.empty) interfaces
  where build (index, dev) m =
          gotNew index dev =<< m


doUpdate :: UHandles -> RoutePacket -> IO ()
doUpdate (mr, f) (Packet hdr msg attrs)
-- for now we will assume that we want the interface
  | messageType hdr == eRTM_NEWLINK = do
    let (Just name) = M.lookup eIFLA_IFNAME attrs
    let names = init $BS.unpack name
    if f names
      then do
        let index = interfaceIndex msg
        m <- readIORef mr
        nm <- gotNew (fromIntegral index) names m
        writeIORef mr nm
      else return ()
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
updaterLoop sock mr = do
  threadWaitRead (getNetlinkFd sock)
  packet <- (recvOne sock :: IO [RoutePacket])
  mapM_ (doUpdate mr) packet
  updaterLoop sock mr


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
