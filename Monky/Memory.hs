{-|
Module      : Monky.Memory
Description : Allows to access information about they systems main memory
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Memory 
(MemoryHandle, getMemoryAvailable, getMemoryHandle, getMemoryTotal,
getMemoryUsed)
where

import Monky.Utility (fopen, readContent, File, findLine)
import Data.Maybe (fromMaybe)


-- |The memory handle used for all functions
data MemoryHandle = MemoryH File

path :: String
path = "/proc/meminfo"

getVal :: String -> Int
getVal = read . (!! 1) . words

-- |Return the memory available to processes
getMemoryAvailable :: MemoryHandle -> IO Int
getMemoryAvailable (MemoryH f) = do
  contents <- readContent f
  return $getVal $fromMaybe "a 0" $findLine "MemAvailable" contents

-- |Get the total amount of memory in the system
getMemoryTotal :: MemoryHandle -> IO Int
getMemoryTotal (MemoryH f) = do
  contents <- readContent f
  return $getVal $fromMaybe "a 0" $findLine "Memtotal" contents

-- |Get the amount of memory used by the kernel and processes
getMemoryUsed :: MemoryHandle -> IO Int
getMemoryUsed h = do
  total <- getMemoryTotal h
  available <- getMemoryAvailable h
  return (total - available)

-- |Get a memory handle
getMemoryHandle :: IO MemoryHandle
getMemoryHandle = do
  file <- fopen path
  return $MemoryH file
