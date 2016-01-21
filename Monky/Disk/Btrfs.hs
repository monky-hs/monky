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
  , getBtrfsHandle )
where

import Monky.Utility
import System.Directory (getDirectoryContents, doesDirectoryExist)

import Monky.Disk.Common

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif


-- Size data metadata system
data BtrfsHandle = BtrfsH Int File File File

instance FsInfo BtrfsHandle where
  getFsSize = return . getSize
  getFsUsed = getUsed


blBasePath :: String
blBasePath = "/sys/block/"

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
  return $dv + mv + sm


-- Filter out the . and .. directory when listing files
filterDirs :: [String] -> [String]
filterDirs = filter (\f -> f /= "." && f /= "..")


mapperToDev :: [String] -> IO [String]
mapperToDev [] = return []
mapperToDev (x:xs) = do
  let path = (blBasePath ++ x ++ "/slaves/")
  tl <- mapperToDev xs
  e <- doesDirectoryExist path
  hd <- if e
    then filterDirs <$> getDirectoryContents path
    else return [x]
  return (hd ++ tl)


getBtrfsHandle' :: String -> IO (BtrfsHandle, [String])
getBtrfsHandle' fs = do
  let devP = fsBasePath ++ fs ++ "/devices/"
  devices <- mapperToDev =<< filterDirs <$> getDirectoryContents devP
  sizes <- mapM (\dev -> fmap read $readFile (blBasePath ++ dev ++ "/size")) devices
  let size = foldl (+) 0 sizes
  d <- fopen (fsBasePath ++ fs ++ "/allocation/data/bytes_used")
  m <- fopen (fsBasePath ++ fs ++ "/allocation/metadata/bytes_used")
  s <- fopen (fsBasePath ++ fs ++ "/allocation/system/bytes_used")
  return (BtrfsH (size*sectorSize) d m s, devices)


getBtrfsHandle :: String -> IO (Maybe (BtrfsHandle, [String]))
getBtrfsHandle fs = do
  e <- doesDirectoryExist (fsBasePath ++ fs)
  if e
    then Just <$> getBtrfsHandle' fs
    else return Nothing
