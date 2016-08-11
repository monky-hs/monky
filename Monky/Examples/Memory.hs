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
Module      : Monky.Examples.Memory
Description : An example module instance for the memory module
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

For older kernels the memory available value may be wrong.
When not provided by the kernel it's computed as `free + cached + buffers`.
If you find this to be to inacurate, use the version for memory free.
But be aware, that free memory on linux can go close to zero because of io buffers.
-}
module Monky.Examples.Memory
  ( getMemoryHandle
  , getMemoryBarHandle
  , getMemoryFreeHandle

  , MHandle
  , MFHandle
  , MBHandle
  )
where

import Monky.Examples.Utility
import Monky.Examples.Images
import Monky.Modules

import Monky.Memory hiding (getMemoryHandle)
import qualified Monky.Memory as M (getMemoryHandle)

-- |Simple handle to display current memory available
newtype MHandle = MH MemoryHandle

-- |Get the the memory handle (available)
getMemoryHandle :: IO MHandle
getMemoryHandle = fmap MH $ M.getMemoryHandle

{- Memory Module -}
instance PollModule MHandle where
  getOutput (MH h) = do
    mp <- getMemoryAvailable h
    return
      [ memoryImage
      , MonkyPlain $ convertUnitB (mp * 1024) "B"
      ]


-- |Simple handle to display current free memory
newtype MFHandle = MFH MemoryHandle

-- |Get the the memory handle (free)
getMemoryFreeHandle :: IO MFHandle
getMemoryFreeHandle = fmap MFH $ M.getMemoryHandle

{- Memory Module -}
instance PollModule MFHandle where
  getOutput (MFH h) = do
    mp <- getMemoryFree h
    return
      [ MonkyImage "mem" '🐏'
      , MonkyPlain $ convertUnitB (mp * 1024) "B"
      ]

-- |Handle to display the current memory usage (used/caches/free) as horizontal bar
data MBHandle = MBH Float MemoryHandle

-- |Get the 'MBHandle'
getMemoryBarHandle
  :: Float -- ^A factor to modify bar length. Total-length: 100/f
  -> IO MBHandle
getMemoryBarHandle f = fmap (MBH f) $ M.getMemoryHandle


getUsagePercents :: MemoryHandle -> IO (Int,Int)
getUsagePercents h = do
  (total, avail, free, _) <- getMemoryStats h
  return ((total - avail) * 100 `div` total, (total - free) * 100 `div` total)

newBar :: Float -> (Int, Int) -> [MonkyOut]
newBar f (u, us) =
  let used   = round $ fromIntegral u / f
      cached = round $ fromIntegral (us - u) / f
      fill   = round (100 / f) - used - cached in
    [ MonkyHBar used
    , MonkyColor ("#444444", "") (MonkyHBar cached)
    , MonkyColor ("#262626", "") (MonkyHBar fill)
    ]

getNMemoryOut :: MemoryHandle -> Float -> IO [MonkyOut]
getNMemoryOut h f = do
  percents <- getUsagePercents h
  return $ memoryImage:newBar f percents

instance PollModule MBHandle where
  getOutput (MBH f h) = getNMemoryOut h f
