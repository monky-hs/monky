module Network  (NetworkHandles, getReadWriteMulti, getNetworkHandles)
where

import Data.Time.Clock.POSIX
import Utility
import Data.IORef
import Control.Monad

data NetworkHandle = NetH File File File (IORef Int) (IORef Int) (IORef POSIXTime)
data NetworkHandles = NetHs [NetworkHandle]

basePath :: String
--basePath = "/sys/class/net/"
basePath = "/sys/class/net/"

readPath :: String
--readPath = "/statistics/rx_bytes"
readPath = "/statistics/rx_bytes"

writePath :: String
writePath = "/statistics/tx_bytes"

statePath :: String
statePath = "/operstate"

getReadWriteReal :: NetworkHandle -> IO (Int, Int)
getReadWriteReal (NetH readf writef _ readref writeref timeref) = do
  read <- readValue readf
  write <- readValue writef
  time <- getPOSIXTime
  oread <- readIORef readref
  owrite <- readIORef writeref
  otime <- readIORef timeref
  let cread = oread - read
  let cwrite = owrite - write
  let ctime = (otime - time)
  writeIORef readref read
  writeIORef writeref write
  writeIORef timeref time
  return ((cread * 8) `div` (round ctime),
    (cwrite * 8) `div` (round ctime))

getReadWrite :: NetworkHandle -> IO (Maybe (Int, Int))
getReadWrite (NetH readf writef statef readref writeref timeref) = do
  state <- readLine statef
  if state == "down"
  then return Nothing
  else do
    val <- getReadWriteReal (NetH readf writef statef readref writeref timeref)
    return $Just val

getMultiReadWriteInt :: [NetworkHandle] -> IO (Maybe (Int, Int))
getMultiReadWriteInt [] = do return Nothing
getMultiReadWriteInt (x:xs) = do
  val <- getReadWrite x
  case val of
    Nothing -> getMultiReadWriteInt xs
    otherwise -> return val

getReadWriteMulti :: NetworkHandles -> IO (Maybe (Int, Int))
getReadWriteMulti (NetHs xs) = getMultiReadWriteInt xs

getNetworkHandle :: String -> IO NetworkHandle
getNetworkHandle dev = do
  read <- fopen $path ++ readPath
  write <- fopen $path ++ writePath
  state <- fopen $path ++ statePath
  readref <- newIORef (1 :: Int)
  writeref <- newIORef (1 :: Int)
  timeref <- newIORef (0 :: POSIXTime)
  return $NetH read write state readref writeref timeref
  where path = basePath ++ dev

getNetworkHandles :: [String] -> IO NetworkHandles
getNetworkHandles [] = do return $NetHs []
getNetworkHandles (x:xs) = do
  handle <- getNetworkHandle x
  (NetHs ns) <- getNetworkHandles xs
  return (NetHs (handle:ns))
