module Network  (NetworkHandle, getReadWrite, getNetworkHandle)
where

import Data.Time.Clock.POSIX
import Utility
import Data.IORef


data NetworkHandle = NetH File File (IORef Int) (IORef Int) (IORef POSIXTime)

basePath :: String
--basePath = "/sys/class/net/"
basePath = "/sys/class/net/"

readPath :: String
--readPath = "/statistics/rx_bytes"
readPath = "/statistics/rx_bytes"

writePath :: String
writePath = "/statistics/tx_bytes"

getReadWrite :: NetworkHandle -> IO (Int, Int)
getReadWrite (NetH readf writef readref writeref timeref) = do
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


getNetworkHandle :: String -> IO NetworkHandle
getNetworkHandle dev = do
  read <- fopen $path ++ readPath
  write <- fopen $path ++ writePath
  readref <- newIORef (1 :: Int)
  writeref <- newIORef (1 :: Int)
  timeref <- newIORef (0 :: POSIXTime)
  return $NetH read write readref writeref timeref
  where path = basePath ++ dev
