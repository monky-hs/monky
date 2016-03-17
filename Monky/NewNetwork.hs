{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Monky.NewNetwork
where

import Data.Bits ((.|.))
import System.IO.Error (catchIOError)
import System.IO (hPutStrLn, stderr)
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

data NetState = Down | Up | Unknown

data NetworkHandle = NetH File File File (IORef Int) (IORef Int) (IORef POSIXTime)

type NetHandle = (String, NetworkHandle)

type Handles = IntMap NetHandle

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


getState :: NetworkHandle -> IO NetState
getState (NetH _ _ statef _ _ _) = do
-- the read can thro an exception if the interace disapperad, we just consider it down
  state <- catchIOError (readLine statef) (\_ -> return "down")
  return $ case state of
    "up" -> Up
    "down" -> Down
    "unknown" -> Unknown
    _ -> error ("Don't know the network state \"" ++ state ++ "\" yet")


getReadWriteM :: NetworkHandle -> IO (Maybe (Int, Int))
getReadWriteM h = do
  state <- getState h
  case state of
    Up -> Just <$> getReadWrite h
    Down -> return Nothing
    Unknown -> Just <$> getReadWrite h


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


getMultiReadWrite :: Handles -> IO (Maybe (Int, Int))
getMultiReadWrite h = do
  hPutStrLn stderr $IM.showTree h
  foldr (\(_, v) -> foldF v) (return start) h
  where start = Just (0, 0)


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

-- TODO check this
closeNetworkHandle :: NetworkHandle -> IO ()
closeNetworkHandle (NetH readf writef statef _ _ _) =
  fclose readf >> fclose writef >> fclose statef


gotNew :: Int -> String -> Handles -> IO Handles
gotNew index name m = do
  case IM.lookup index m of
    Nothing -> do
      hPutStrLn stderr ("Creating new interface: " ++ name)
      h <- getNetworkHandle name
      return $IM.insert index (name, h) m
    Just (x, v) -> if x == name
      then return m
      else do
        hPutStrLn stderr ("Replacing " ++ x ++ " with " ++ name)
        h <- getNetworkHandle name
        closeNetworkHandle v
        return $IM.adjust (\_ -> (name, h)) index m


lostOld :: Int -> Handles -> IO Handles
lostOld index m = case IM.lookup index m of
  Nothing -> return m
  (Just (_, h)) ->
    closeNetworkHandle h >>
    return (IM.delete index m) -- TOOD close handle


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


getCurrentDevs :: IO [(Int, String)]
getCurrentDevs = do
  sock <- makeSocket
  ifs <- query sock requestPacket
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
    hPutStrLn stderr (names ++ " appeared")
    if f names
      then do
        let index = interfaceIndex msg
        m <- readIORef mr
        nm <- gotNew (fromIntegral index) names m
        writeIORef mr nm
      else return ()
  | messageType hdr == eRTM_DELLINK = do
    let (Just name) = M.lookup eIFLA_IFNAME attrs
    let names = init $BS.unpack name
    hPutStrLn stderr (names ++ " disappeared")
    let index = interfaceIndex msg
    m <- readIORef mr
    nm <- lostOld (fromIntegral index) m
    writeIORef mr nm
  | otherwise = return ()
-- Ignore everything else
doUpdate _ _ = return ()


updaterLoop :: NetlinkSocket -> UHandles -> IO ()
updaterLoop sock mr = do
  threadWaitRead (getNetlinkFd sock)
  packet <- (recvOne sock :: IO [RoutePacket])
  mapM_ (doUpdate mr) packet
  updaterLoop sock mr


updater :: UHandles -> IO ()
updater h = do
  sock <- makeSocket
  joinMulticastGroup sock 1
  updaterLoop sock h


getUHandles :: (String -> Bool) -> IO UHandles
getUHandles f = do
  handle <- getNetworkHandles f
  ref <- newIORef handle
  _ <- forkIO (updater (ref, f))
  return (ref, f)
