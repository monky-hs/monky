module Disk  (DiskHandle, getDiskReadWrite, getDiskFree, getDiskHandle)
where

import Data.Time.Clock.POSIX
import Utility


data DiskHandle = BTRFSH String Int Int Int Int POSIXTime | Empty

path :: String
path = "/proc/diskstats"

basePath :: String
basePath = "/sys/block/"

fsBasePath :: String
fsBasePath = "/sys/fs/btrfs/"

fsUUID :: String
fsUUID = "cb592ffc-c82e-4e14-b513-45358e7d4b93"

sectorSize :: Int
sectorSize = 512

getDiskReadWrite :: DiskHandle -> IO (Int, Int, DiskHandle)
getDiskReadWrite Empty = do return (0, 0, Empty)
getDiskReadWrite (BTRFSH dev part size oread owrite otime) = do
  content <- readFile path
  time <- getPOSIXTime
  let values = map read (drop 1 (filter (\l -> (head l) == "dm-0") (map (drop 2) (map words (lines content))) !! 0)) :: [Int]
  let read = (values !! 2) * sectorSize
  let write = (values !! 6) * sectorSize
  let cread = read - oread
  let cwrite = write - owrite
  let ctime = time - otime
  return (div cread (round ctime), div cwrite (round ctime), (BTRFSH dev part size read write time))

getDiskFree :: DiskHandle -> IO (Int, DiskHandle)
getDiskFree Empty = do return (0, Empty)
getDiskFree (BTRFSH dev part size oread owrite otime) = do
  dused <- readFile (fsBasePath ++ fsUUID ++ "/allocation/data/bytes_used")
  mused <- readFile (fsBasePath ++ fsUUID ++ "/allocation/metadata/bytes_used")
  sused <- readFile (fsBasePath ++ fsUUID ++ "/allocation/system/bytes_used")
  let dfree = size - (read dused :: Int) - (read mused :: Int) - (read sused :: Int)
  return (div dfree 1000000000, (BTRFSH dev part size oread owrite otime))

getDiskHandle :: String -> Int -> IO DiskHandle
getDiskHandle "" 0 = do return Empty
getDiskHandle dev part = do
  t <- getPOSIXTime
  content <- readFile (basePath ++ dev ++ "/" ++ dev ++ (show part) ++ "/size")
  let size = (read content :: Int) * sectorSize
  return $BTRFSH dev part size 0 0 0
