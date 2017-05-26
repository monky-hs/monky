{-
    Copyright 2015,2016 Markus Ongyerth, Stephan Guenther

    This file is part of Monky.

    Monky is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Monky is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Monky.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Memory
Description : Allows to access information about they systems main memory
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Memory
  ( MemoryHandle
  , getMemoryAvailable
  , getMemoryHandle
  , getMemoryTotal
  , getMemoryUsed
  , getMemoryFree
  , getMemoryStats
  )
where

import Monky.Utility (fopen, readContent, File)
import Data.Maybe (fromMaybe, listToMaybe)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS


-- |The memory handle used for all functions
newtype MemoryHandle = MemoryH File

path :: String
path = "/proc/meminfo"

getVal :: ByteString -> Int
getVal = fst . fromMaybe (error "Failed to read as int in memory module") .  BS.readInt . (!! 1) . BS.words


findLine :: ByteString -> [ByteString] -> Maybe ByteString
findLine x = listToMaybe . filter (x `BS.isPrefixOf`)

{- |Return the memory available to userspace

This is accurate (read from kernel) for current kernels.
Old kernel (~3.13) estimates this. Old kernels may overestimate.
-}
getMemoryAvailable :: MemoryHandle -> IO Int
getMemoryAvailable (MemoryH f) = do
  contents <- readContent f
  let line =  findLine "MemAvailable" contents
  case line of

    Nothing -> do
      let buffs = fromMaybe err $ findLine "Buffers" contents
          cached = fromMaybe err $ findLine "Cached" contents
      free <- getMemoryFree (MemoryH f)
      return $ getVal buffs + getVal cached + free
    (Just x) -> return . getVal $ x
  where err = error "Could not find one of the fallback values for MemAvailable, please report this together with the content of /proc/meminfo"

-- |Get the total amount of memory in the system
getMemoryTotal :: MemoryHandle -> IO Int
getMemoryTotal (MemoryH f) = do
  contents <- readContent f
  return . getVal . fromMaybe err . findLine "MemTotal" $ contents
    where err = error "Could not find MemTotal in /proc/meminfo. Please report this bug with the content of /proc/meminfo"


-- |Get the amount of memory rported as free by the kernel
getMemoryFree :: MemoryHandle -> IO Int
getMemoryFree (MemoryH f) = do
    contents <- readContent f
    return . getVal . fromMaybe err . findLine "MemFree" $ contents
    where err = error "Could not find MemFree in /proc/meminfo. Please report this bug with the content of /proc/meminfo"

-- |Get the amount of memory used by the kernel and processes
getMemoryUsed :: MemoryHandle -> IO Int
getMemoryUsed h = do
  (_, _, _, used) <- getMemoryStats h
  return used

-- |Get memory statistics in one got (with only one read) (total, avail, free, used)
getMemoryStats :: MemoryHandle -> IO (Int, Int, Int, Int)
getMemoryStats (MemoryH f) = do
  contents <- readContent f
  let avail = getVal . fromMaybe "a 0" . findLine "MemAvailable" $ contents
      total = getVal . fromMaybe "a 0" . findLine "MemTotal" $ contents
      free = getVal . fromMaybe "a 0" . findLine "MemFree" $ contents
      used = total - avail
  return (total, avail, free, used)


-- |Get a memory handle
getMemoryHandle :: IO MemoryHandle
getMemoryHandle = do
  file <- fopen path
  return $MemoryH file
