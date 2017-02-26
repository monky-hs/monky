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
  ( BlockHandle(..)
  , getBlockHandle
  , getBlockHandleTag

  , devToMount
  )
where

import System.Posix.StatVFS
import Data.Maybe (listToMaybe)
import Data.List (isSuffixOf)

import Monky.Blkid
import Monky.Disk.Common

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif


{- |Get "the" mountpoint of a device. 

Returns a mointpoint of a device. If there are multiple mountpoints, this will
return the first one found.
First one is mostly determined by order in /proc/mounts and should be the one
that was mounted first (time since boot).
-}
devToMount :: Dev -> IO (Maybe String)
devToMount dev = do
  masters <- devToMapper dev
  mounts <- map (take 2 . words) . lines <$> readFile "/proc/mounts"
  return . listToMaybe . map (!! 1) $ filter (isDev masters) mounts
  where
    isDev masters [x, _] = any (\(Label master) -> ('/':master) `isSuffixOf` x) masters
    isDev _ _ = error "devToMount: How does take 2 not match [_, _]?"


-- Size data metadata system
-- |The FsInfo handle exported by this module
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
  return $fromIntegral (fromIntegral (statVFS_bavail fstat) * statVFS_frsize fstat)


getBlockHandle' :: Dev -> IO (Maybe (BlockHandle, Dev))
getBlockHandle' dev = do
  path <- devToMount dev
  case path of
        Just x -> return $Just (BlockH x, dev)
        Nothing -> return Nothing

{- |Get a fs handle for 'normal' devices

This uses fsStat to get file system information.

fsStat takes the mount point of the file system, so we need to find the mount point.

In case of mapper devices, this is done by going through the chain of slaves.
-}
getBlockHandle :: String -> IO (Maybe (BlockHandle, Dev))
getBlockHandle = getBlockHandleTag "UUID"

-- | Same as 'getBlockHandle' but allow to pass the tag for libblkid
getBlockHandleTag :: String -> String -> IO (Maybe (BlockHandle, Dev))
getBlockHandleTag t fs = do
  dev <- evaluateTag t fs
  case dev of
    Just x -> do
        y <- labelToDev (Label x)
        getBlockHandle' y
    Nothing -> return Nothing
