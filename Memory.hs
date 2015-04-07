module Memory  (MemoryHandle, getMemoryAvailable, getMemoryHandle)
where

import Utility

data MemoryHandle = MemoryH File Int Int

path :: String
path = "/proc/meminfo"

getMemoryAvailable :: MemoryHandle -> IO (Int, MemoryHandle)
getMemoryAvailable (MemoryH f a w) = do
  cached <- readIntInLineStartingWith f "Cached:"
  total <- readIntInLineStartingWith f "MemTotal:"
  free <- readIntInLineStartingWith f "MemFree:"
  let mem = (total-cached-free)*1000
  return (mem, MemoryH f 0 0)

getMemoryHandle :: IO MemoryHandle
getMemoryHandle = do
  file <- fopen $path
  return $MemoryH file 0 0
