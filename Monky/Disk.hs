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
Module      : Monky.Disk
Description : Allows access to information about a btrfs pool
Maintainer  : moepi
Stability   : experimental
Portability : Linux

This module allows for some support for btrfs devices.
This may be renamed in the future when a general block-device module appears.
-}
module Monky.Disk
  ( DiskHandle
  , getDiskReadWrite
  , getDiskFree
  , getDiskHandle
  , getDiskHandleTag
  )
where

import Monky.Utility
import Data.Time.Clock.POSIX
import Data.IORef

import Monky.Disk.Common
import Monky.Disk.Btrfs
import Monky.Disk.Device

-- |The handle xported by this module
-- A disk may have multiple physical devices so we use lists for them
data DiskHandle = DiskH FSI [File] [IORef Int] [IORef Int] (IORef POSIXTime)


sectorSize :: Int
sectorSize = 512


-- |Get the read write rates from the disk (in bytes/s)
getDiskReadWrite :: DiskHandle -> IO (Int, Int)
getDiskReadWrite (DiskH _ fs readrefs writerefs timeref) = do
  contents <- mapM readValues fs
  time <- getPOSIXTime
  let nreads = map (\c -> (c !! 2) * sectorSize) contents
  let writes = map (\c -> (c !! 6) * sectorSize) contents
  oreads <- mapM readIORef readrefs
  owrites <- mapM readIORef writerefs
  otime <- readIORef timeref
  let creads = zipWith (-) nreads oreads
  let cwrites = zipWith (-) writes owrites
  let ctime = time - otime
  _ <- sequence $zipWith writeIORef readrefs nreads
  _ <- sequence $zipWith writeIORef writerefs writes
  writeIORef timeref time
  return (sum $map (`sdivBound` round ctime) creads, sum $map (`sdivBound` round ctime) cwrites)


-- |Get the space left on the disk
getDiskFree :: DiskHandle -> IO Int
getDiskFree (DiskH (FSI h) _ _ _ _) = getFsFree h


getBtrfsDH :: (BtrfsHandle, [Dev]) -> IO DiskHandle
getBtrfsDH (h, devs) = do
  -- Open the stat file for each physical device
  fs <- mapM (\(Dev dev) -> fopen (blBasePath ++ dev ++ "/stat")) devs
  -- this gets the right number of IORefs without number hacking
  wfs <- mapM (\_ -> newIORef 0) devs
  rfs <- mapM (\_ -> newIORef 0) devs
  t <- newIORef 0
  return (DiskH (FSI h) fs wfs rfs t)


getBlockDH :: (BlockHandle, Dev) -> IO DiskHandle
getBlockDH (h, Dev dev) = do
  f <- fopen (blBasePath ++ dev ++ "/stat")
  wf <- newIORef 0
  rf <- newIORef 0
  t <- newIORef 0
  return (DiskH (FSI h) [f] [wf] [rf] t)


-- | Get a disk handle from uuid. This special-cases btrfs.
getDiskHandle :: String -> IO DiskHandle
getDiskHandle uuid = do
-- First try btrfs file systems
  btrfs <- getBtrfsHandle uuid
  case btrfs of
    (Just x) -> getBtrfsDH x
    Nothing -> getDiskHandleTag "UUID" uuid

-- | Get the disk handle from a user chosen blkid tag.
getDiskHandleTag :: String -> String -> IO DiskHandle
getDiskHandleTag t v = do
    block <- getBlockHandleTag t v
    case block of
        Just x -> getBlockDH x
        Nothing -> error "Disk currently does not support your setup"
