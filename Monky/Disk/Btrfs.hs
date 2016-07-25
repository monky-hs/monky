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
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.Disk.Btrfs
Description : Allows access to information about a btrfs pool
Maintainer  : ongy
Stability   : experimental
Portability : Linux

This module allows for some support for btrfs devices.
This may be renamed in the future when a general block-device module appears.
-}
module Monky.Disk.Btrfs
  ( BtrfsHandle
  , getBtrfsHandle
  , getFSDevices
  )
where

import Monky.Utility
import System.Directory (listDirectory, doesDirectoryExist)

import Monky.Disk.Common

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif


-- Size data metadata system
-- |The FsInfo exported by this module
data BtrfsHandle = BtrfsH Int File File File

instance FsInfo BtrfsHandle where
  getFsSize = return . getSize
  getFsUsed = getUsed


fsBasePath :: String
fsBasePath = "/sys/fs/btrfs/"


sectorSize :: Int
sectorSize = 512


getSize :: BtrfsHandle -> Int
getSize (BtrfsH s _ _ _) = s


getUsed :: BtrfsHandle -> IO Int
getUsed (BtrfsH _ d m s) = do
  dv <- readValue d
  mv <- readValue m
  sm <- readValue s
  return $ dv + mv + sm

-- |Get the block devices used by a btrfs FileSystem. This resolves mappers as far as possible
getFSDevices :: String -> IO [String]
getFSDevices fs = do
  let devP = fsBasePath ++ fs ++ "/devices/"
  concat <$> (mapM mapperToDev =<< listDirectory devP)


getBtrfsHandle' :: String -> IO BtrfsHandle
getBtrfsHandle' fs = do
  devices <- getFSDevices fs
  sizes <- mapM (\dev -> fmap read $ readFile (blBasePath ++ dev ++ "/size")) devices
  let size = sum sizes
  d <- fopen (fsBasePath ++ fs ++ "/allocation/data/bytes_used")
  m <- fopen (fsBasePath ++ fs ++ "/allocation/metadata/bytes_used")
  s <- fopen (fsBasePath ++ fs ++ "/allocation/system/bytes_used")
  return (BtrfsH (size*sectorSize) d m s)

{-| Try to create a btfshanlde given the UUID

This will create a 'BtrfsHandle' which is an instance of FsInfo and
a list of block devices that are slaves of our file system.

This allows the upper layer to monitor the read/write rates of all
block devices that belong to our file system and report them as
read/write rate for the file system.

Due to compression and encryption the read/write rate on the block
device may be quite different to the one that application see.
-}
getBtrfsHandle
  :: String -- ^The UUID of the file system to monitor
  -> IO (Maybe (BtrfsHandle, [String]))
getBtrfsHandle fs = do
  e <- doesDirectoryExist (fsBasePath ++ fs)
  if e
    then do
      h <- getBtrfsHandle' fs
      devs <- getFSDevices fs
      return $ Just (h, devs)
    else return Nothing
