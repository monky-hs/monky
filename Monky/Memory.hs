{-|
Module      : Memory
Description : Allows to access information about they systems main memory
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Memory (MemoryHandle, getMemoryAvailable, getMemoryHandle)
where

import Monky.Utility (fopen, readContent, File)
import Data.List (isPrefixOf)


-- |The memory handle used for all functions
data MemoryHandle = MemoryH File

path :: String
path = "/proc/meminfo"

{- |Return the memroy available to the system

  This is calculate by: (total - cached - free)
-}
getMemoryAvailable :: MemoryHandle -> IO Int
getMemoryAvailable (MemoryH f) = do
  contents <- readContent f
  let cached = getVal $findLine "Cached:" contents
  let total  = getVal $findLine "MemTotal:" contents
  let free   = getVal $findLine "MemFree:" contents
  let mem = (total - cached - free) * 1000
  return mem
  where
    findLine y (x:xs) = if isPrefixOf y x then x else findLine y xs
    findLine y [] = y ++ "0"
    getVal = read . (!! 1) . words

-- |Get a memory handle
getMemoryHandle :: IO MemoryHandle
getMemoryHandle = do
  file <- fopen path
  return $MemoryH file
