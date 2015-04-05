module Disk  (DiskHandle, getDiskReadWrite, getDiskHandle)
where

import Data.Time.Clock.POSIX
import Utility


data DiskHandle = DiskH String Int Int POSIXTime

path :: String
path = "/proc/diskstats"

sectorSize :: Int
sectorSize = 64

getDiskReadWrite :: DiskHandle -> IO (Int, Int, DiskHandle)
getDiskReadWrite (DiskH dev oread owrite otime) = do
  content <- readFile path
  time <- getPOSIXTime
  let values = map read (drop 1 (filter (\l -> (head l) == "dm-0") (map (drop 2) (map words (lines content))) !! 0)) :: [Int]
  let read = (values !! 3) * sectorSize
  let write = (values !! 7) * sectorSize
  let cread = read - oread
  let cwrite = write - owrite
  let ctime = time - otime
  return (div cread (round ctime), div cwrite (round ctime), (DiskH dev read write time))

getDiskHandle :: String -> IO DiskHandle
getDiskHandle dev = do
  t <- getPOSIXTime
  return $DiskH dev 0 0 0
