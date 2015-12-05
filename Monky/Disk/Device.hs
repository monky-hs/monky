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
Module      : Monky.Disk.Device
Description : Allows access to information about generic block device
Maintainer  : ongy
Stability   : experimental
Portability : Linux

This module allows to read generic information about a block device
and its file system.

It only works if the file system is mounted when the handle is created
since it needs to *find* the mount point to get information about the
file system
-}
module Monky.Disk.Device
  ( BlockHandle
  , getBlockHandle )
where

import System.Posix.StatVFS
import Data.Maybe (listToMaybe, catMaybes)
import Data.List (isPrefixOf)
import System.Directory (getDirectoryContents)
import System.Posix.Files (readSymbolicLink)

import Monky.Blkid
import Monky.Disk.Common

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif


devToMount :: String -> IO (Maybe String)
devToMount dev = do
  mounts <- map (take 2 . words) . lines <$> readFile "/proc/mounts"
  return . listToMaybe . map (!! 1) $filter isDev mounts
  where
    isDev = isPrefixOf (reverse ('/':dev)) . reverse . head


getMapperDevs :: IO [(String, String)]
getMapperDevs = do
  devs <- mapM readLink =<< filter isDev <$> getDirectoryContents "/dev/mapper/"
  return $map getName devs
  where
    readLink l = (,) l <$> readSymbolicLink ("/dev/mapper/" ++ l)
    isDev f = f /= "control" && f /= "." && f /= ".."
    getName (d, p) = (d, reverse . takeWhile (/= '/') . reverse $p)


isSlaveOf :: String -> String -> IO Bool
isSlaveOf slave dev =
  elem slave <$> getDirectoryContents ("/sys/block/" ++ dev ++ "/slaves/")


devToMountM :: String -> IO (Maybe String)
devToMountM dev = do
  path <- devToMount dev
  case path of
    Nothing -> do
      devs <- getMapperDevs
      mdev <- mapM (\(l,m) -> dev `isSlaveOf` m >>= (\b -> return (if b then Just (l,m) else Nothing))) devs
      rdev <- mapM (devToMountM . fst) $catMaybes mdev
      return (listToMaybe $catMaybes rdev)
    Just x -> return $Just x



-- Size data metadata system
data BlockHandle = BlockH FilePath

instance FsInfo BlockHandle where
  getFsSize = getSize
  getFsFree = getFree



getSize :: BlockHandle -> IO Int
getSize (BlockH path) = do
  fstat <- statVFS path
  return $fromIntegral (fromIntegral (statVFS_blocks fstat) * statVFS_frsize fstat)

getFree :: BlockHandle -> IO Int
getFree (BlockH path) = do
  fstat <- statVFS path
  return $fromIntegral (fromIntegral (statVFS_bfree fstat) * statVFS_bsize fstat)


getBlockHandle' :: String -> IO (Maybe (BlockHandle, String))
getBlockHandle' dev = do
  path <- devToMountM dev
  case path of
    Just x -> return $Just (BlockH x, dev)
    Nothing -> return Nothing

getBlockHandle :: String -> IO (Maybe (BlockHandle, String))
getBlockHandle fs = do
  dev <- evaluateTag "UUID" fs
  case dev of
    Just x -> getBlockHandle' (reverse $takeWhile (/= '/') $reverse x)
    Nothing -> return Nothing
