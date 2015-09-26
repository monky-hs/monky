{-
    Copyright 2015 Markus Ongyerth, Stephan Guenther

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
  return $getVal $fromMaybe "a 0" $findLine "MemTotal" contents

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
