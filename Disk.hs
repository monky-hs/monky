module Disk  (DiskHandle, getDiskReadWrite, getDiskFree, getDiskHandle)
where

import Data.Time.Clock.POSIX
import Utility


data DiskHandle = DiskH String Int Int Int Int POSIXTime

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
getDiskReadWrite (DiskH dev part size oread owrite otime) = do
  content <- readFile path
  time <- getPOSIXTime
  let values = map read (drop 1 (filter (\l -> (head l) == "dm-0") (map (drop 2) (map words (lines content))) !! 0)) :: [Int]
  let read = (values !! 2) * sectorSize
  let write = (values !! 6) * sectorSize
  let cread = read - oread
  let cwrite = write - owrite
  let ctime = time - otime
  return (div cread (round ctime), div cwrite (round ctime), (DiskH dev part size read write time))

getDiskFree :: DiskHandle -> IO (Int, DiskHandle)
getDiskFree (DiskH dev part size oread owrite otime) = do
  dused <- readFile (fsBasePath ++ fsUUID ++ "/allocation/data/bytes_used")
  mused <- readFile (fsBasePath ++ fsUUID ++ "/allocation/metadata/bytes_used")
  sused <- readFile (fsBasePath ++ fsUUID ++ "/allocation/system/bytes_used")
  let dfree = size - (read dused :: Int) - (read mused :: Int) - (read sused :: Int)
  return (div dfree 1000000000, (DiskH dev part size oread owrite otime))

getDiskHandle :: String -> Int -> IO DiskHandle
getDiskHandle dev part = do
  t <- getPOSIXTime
  content <- readFile (basePath ++ dev ++ "/" ++ dev ++ (show part) ++ "/size")
  let size = (read content :: Int) * sectorSize
  return $DiskH dev part size 0 0 0
