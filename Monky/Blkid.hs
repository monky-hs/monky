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
{-|
Module      : Monky.Blkid
Description : Minimal access to liblkid
Maintainer  : ongy
Stability   : experimental
Portability : Linux

This module allows access to libblkid functionality

For now we will just dyniamically link against it.
If there are systems that do not provide liblkid this will change.
-}
module Monky.Blkid 
  ( evaluateTag
  , evaluateSpec
  )
where

import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc

data Null

foreign import ccall "blkid_evaluate_tag" c_evt :: CString -> CString -> Ptr Null -> IO CString
foreign import ccall "blkid_evaluate_spec" c_evs :: CString -> Ptr Null -> IO CString

evaluateTag :: String -> String -> IO String
evaluateTag t v = do
  ptr <- withCString t (\ct -> withCString v (\cv -> c_evt ct cv nullPtr))
  ret <- peekCString ptr
  free ptr
  return ret

evaluateSpec :: String -> IO String
evaluateSpec s = do
  ptr <- withCString s (\cs -> c_evs cs nullPtr)
  ret <- peekCString ptr
  free ptr
  return ret

