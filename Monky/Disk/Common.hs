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
  )
where


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

data FSI = forall a. FsInfo a => FSI a

fsToFSI :: FsInfo a => a -> FSI
fsToFSI = FSI
