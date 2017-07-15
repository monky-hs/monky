{-
    Copyright 2015,2017 Markus Ongyerth, Stephan Guenther

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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.Disk.Common
Description : Provides the common disk interfaces
Maintainer  : moepi
Stability   : experimental
Portability : Linux

This provides the class used by file system specific implementations
-}

module Monky.Disk.Common
  ( FSI(FSI)
  , FsInfo(..)
  , fsToFSI

  , blBasePath
  , devToMapper
  , mapperToDev
  , Dev (..)
  , Label (..)
  , labelToDev
  )
where

import qualified Data.Map as M
import Data.Map (Map)
import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Monky.Utility
import System.Directory (doesDirectoryExist)
import Data.List (nub, sort)
import System.Posix.Files

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

newtype Label = Label { getLabel :: String } deriving (Show, Eq, Ord)
newtype Dev = Dev { getDev :: String } deriving (Show, Eq, Ord)

ltd :: Label -> Dev
ltd (Label x) = Dev x

dtl :: Dev -> Label
dtl (Dev x) = Label x

-- | Get the real device (e.g. dm-0) that's behind a device name
getMapperDev :: Label -> IO Dev
getMapperDev (Label dev) = do
    let devPath = "/dev/mapper/" ++ dev
    stat <- getSymbolicLinkStatus devPath
    Dev <$> if isSymbolicLink stat
        then reverse . takeWhile (/= '/') . reverse <$> readSymbolicLink devPath
        else if isBlockDevice stat
            then return ("dm-" ++ (show . snd $ statToMM stat))
            else error msg
    where msg = "The disk resolution is a bit buggy currently please make a bug report with an `ls -lh` of your /dev/mapper"

--  This could in theory be a global
getLabelPairs :: IO [(Label, Dev)]
getLabelPairs = do
    devs <- filter (/= "control") <$> listDirectory "/dev/mapper/"
    let labels = map Label devs
    mapM (\s -> (\d -> (s, d)) <$> getMapperDev s) labels

getLabelMap :: IO (Map Label Dev)
getLabelMap = M.fromList <$> getLabelPairs

getDeviceMap :: IO (Map Dev Label)
getDeviceMap = M.fromList . map swap <$> getLabelPairs

labelToDev' :: Map Label Dev -> Label -> Dev
labelToDev' m l =
    fromMaybe (ltd l) $ M.lookup l m

devToLabel' :: Map Dev Label -> Dev -> Label
devToLabel' m d =
    fromMaybe (dtl d) $ M.lookup d m

labelToDev :: Label -> IO Dev
labelToDev l = do
    m <- getLabelMap
    return $ labelToDev' m l

{-| Type class that should be instanciated by file system handlers

The Monky.Disk module is designed to work with different handlers
specialized for different file systems and a generic block device
handler.

This typeclass gives the interface a file system handler has to
implement to be usable.
-}
class FsInfo a where
  -- |Get the bytes free on the file system
  getFsFree :: a -> IO Integer
  getFsFree h = do
    s <- getFsSize h
    u <- getFsUsed h
    return (s - u)
  -- |Get the total size of the file system
  getFsSize :: a -> IO Integer
  getFsSize h = do
    u <- getFsUsed h
    f <- getFsFree h
    return (u + f)
  -- |Get the bytes used by the file system
  getFsUsed :: a -> IO Integer
  getFsUsed h = do
    s <- getFsSize h
    f <- getFsFree h
    return (s - f)
  -- |Get all data, might be more efficient
  -- (Size, Free, Used)
  getFsAll :: a -> IO (Integer, Integer, Integer)
  getFsAll h = do
    s <- getFsSize h
    f <- getFsFree h
    u <- getFsUsed h
    return (s, f, u)

-- |Existential datatype to wrap 'FsInfo' instances
data FSI = forall a. FsInfo a => FSI a

-- |Wrap a 'FsInfo' into an 'FSI'
fsToFSI :: FsInfo a => a -> FSI
fsToFSI = FSI

-- |The base path of block devices on the system
blBasePath :: String
blBasePath = "/sys/class/block/"

-- | Go from the FileStatus record to Major and Minor device number
statToMM :: FileStatus -> (Int, Int)
statToMM stat =
    let both = fromIntegral . specialDeviceID $ stat
     in (both `shiftR` 8, both .&. 8)

-- | Traverse the device tree downwards (find all devices used) in kernel
-- | devcice name hirarchy
mapperToDev' :: String -> IO [String]
mapperToDev' x = sort . nub <$> do
  let path = blBasePath ++ x ++ "/slaves/"
  e <- doesDirectoryExist path
  if e
    then do
      rec <- mapM mapperToDev' =<< listDirectory path
      return $ concat rec
    else return [x]

-- | Get the physical block devices supporting some device
mapperToDev :: Label -> IO [Dev]
mapperToDev x = do
    m <- getLabelMap
    let (Dev dev) = fromMaybe (ltd x) $ M.lookup x m
    map Dev <$> mapperToDev' dev

-- | Traverse the device tree upwards (find all users of a device) in
-- | kernel device name hirarchy
devToMapper' :: String -> IO [String]
devToMapper' x = sort . nub <$> do
  let path = blBasePath ++ x ++ "/holders/"
  holders <- listDirectory path
  if null holders
    then return [x]
    else do
      rec <- mapM devToMapper' holders
      return $ concat rec

-- |Get the "top most" virtual device(s) based on the physical device
devToMapper :: Dev -> IO [Label]
devToMapper (Dev x) = do
    m <- getDeviceMap
    map (devToLabel' m . Dev) <$> devToMapper' x
