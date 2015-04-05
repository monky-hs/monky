module Network  (NetworkHandle, getReadWrite, getNetworkHandle)
where

import Data.Time.Clock.POSIX
import Utility


data NetworkHandle = NetH File File Int Int POSIXTime

basePath :: String
--basePath = "/sys/class/net/"
basePath = "/sys/devices/pci0000:00/0000:00:1c.2/0000:03:00.0/net/"

readPath :: String
--readPath = "/statistics/rx_bytes"
readPath = "/statistics/rx_bytes"

writePath :: String
writePath = "/statistics/tx_bytes"

getReadWrite :: NetworkHandle -> IO (Int, Int, NetworkHandle)
getReadWrite (NetH readf writef oread owrite otime) = do
  read <- readValue readf
  write <- readValue writef
  time <- getPOSIXTime
  let cread = oread - read
  let cwrite = owrite - write
  let ctime = (otime - time)
  return ((cread*1000) `div` (round ctime),
    (cwrite*1000) `div` (round ctime),
    (NetH readf writef read write time))
  

getNetworkHandle :: String -> IO NetworkHandle
getNetworkHandle dev = do
  t <- getPOSIXTime
  read <- fopen $path ++ readPath
  write <- fopen $path ++ writePath
  return $NetH read write 0 0 0
  where path = basePath ++ dev
