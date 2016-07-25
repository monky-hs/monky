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
  )
where

import Monky.Utility
import System.Directory (doesDirectoryExist)
import Data.List (nub, sort)

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif


{-| Type class that should be instanciated by file system handlers

The Monky.Disk module is designed to work with different handlers
specialized for different file systems and a generic block device
handler.

This typeclass gives the interface a file system handler has to
implement to be usable.
-}
class FsInfo a where
  -- |Get the bytes free on the file system
  getFsFree :: a -> IO Int
  getFsFree h = do
    s <- getFsSize h
    u <- getFsUsed h
    return (s - u)
  -- |Get the total size of the file system
  getFsSize :: a -> IO Int
  getFsSize h = do
    u <- getFsUsed h
    f <- getFsFree h
    return (u + f)
  -- |Get the bytes used by the file system
  getFsUsed :: a -> IO Int
  getFsUsed h = do
    s <- getFsSize h
    f <- getFsFree h
    return (s - f)
  -- |Get all data, might be more efficient
  -- (Size, Free, Used)
  getFsAll :: a -> IO (Int, Int, Int)
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

-- |Get the physical block devices supporting some device
mapperToDev :: String -> IO [String]
mapperToDev x = sort . nub <$> do
  let path = blBasePath ++ x ++ "/slaves/"
  e <- doesDirectoryExist path
  if e
    then do
      rec <- mapM mapperToDev =<< listDirectory path
      return $ concat rec
    else return [x]

-- |Get the "top most" virtual device(s) based on the physical device
devToMapper :: String -> IO [String]
devToMapper x = sort . nub <$> do
  let path = blBasePath ++ x ++ "/holders/"
  holders <- listDirectory path
  if null holders
    then return [x]
    else do
      rec <- mapM devToMapper holders
      return $ concat rec
