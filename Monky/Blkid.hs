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
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.Blkid
Description : Minimal access to liblkid
Maintainer  : ongy
Stability   : experimental
Portability : Linux

This module allows access to libblkid functionality

Since the library does not have to exist on every system we link
against it on runtime when needed.
We don't have a handle or something cool like that, so we will just
load it when needed und unload again after
-}
module Monky.Blkid
  ( evaluateTag
  , evaluateSpec
  , evaluateTag'
  , evaluateSpec'
  , withLibBlkid
  )
where

import Monky.Template

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif


data Cache

importLib "LibBlkid" "libblkid.so.1"
  [ ( "c_evt", "blkid_evaluate_tag", "CString -> CString -> Ptr Cache -> IO CString")
  , ( "c_evs", "blkid_evaluate_spec", "CString -> Ptr Cache -> IO CString")
  ]

getAndFreeString :: CString -> IO String
getAndFreeString ptr = do
  ret <- peekCString ptr
  free ptr
  return ret


maybeGetString :: CString -> IO (Maybe String)
maybeGetString ptr = if ptr == nullPtr
    then return Nothing
    else Just <$> getAndFreeString ptr

-- |Version to reuse the LibBlkid
evaluateTag' :: String -> String -> LibBlkid -> IO (Maybe String)
evaluateTag' t v l = do
  ptr <- withCString t (\ct -> withCString v (\cv -> c_evt l ct cv nullPtr))
  maybeGetString ptr

-- |version to reuse the LibBlkid
evaluateSpec' :: String -> LibBlkid -> IO (Maybe String)
evaluateSpec' s l = do
  ptr <- withCString s (\cs -> c_evs l cs nullPtr)
  maybeGetString ptr

-- |Evaluate a tag
evaluateTag :: String -> String -> IO (Maybe String)
evaluateTag t v = withLibBlkid $ evaluateTag' t v

-- |Evaluate a spec
evaluateSpec :: String -> IO (Maybe String)
evaluateSpec = withLibBlkid . evaluateSpec'

-- |Execute an IO action with an instance of LibBlkid
withLibBlkid :: (LibBlkid -> IO a) -> IO a
withLibBlkid a = do
  l <- getLibBlkid
  ret <- a l
  destroyLibBlkid l
  return ret
