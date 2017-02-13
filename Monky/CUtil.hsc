{-
    Copyright 2017 Markus Ongyerth

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
{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Monky.CUtil
Description : Provides C utility functions
Maintainer  : ongy
Stability   : testing
Portability : Linux

This provides low-level utility wrappers used by Monky.Util
-}
module Monky.CUtil
    ( UName(..)
    , uname
    )
where

import Foreign.Ptr
import Foreign.C.Types

import Data.Text (Text)
import qualified Data.Text.Foreign as T

import Foreign.Storable
import Foreign.Marshal.Alloc

#include <sys/utsname.h>

foreign import ccall unsafe "strlen" c_strlen :: Ptr CChar -> IO Word
foreign import ccall unsafe "uname" c_uname :: Ptr UName -> IO ()

-- | The haskell type for Cs utsname (man uname)
data UName = UName
    { _uSysName  :: Text
    , _uNodeName :: Text
    , _uRelease  :: Text
    , _uVersion  :: Text
    , _uMachine  :: Text
    } deriving (Eq, Show)

peekCString :: Ptr CChar -> IO Text
peekCString ptr = do
    len <- c_strlen ptr
    T.peekCStringLen (ptr, fromIntegral len)

instance Storable UName where
    sizeOf _ = #{size struct utsname}
    alignment _ = alignment (undefined :: CLong)
    peek p = UName
        <$> peekCString (#{ptr struct utsname, sysname} p)
        <*> peekCString (#{ptr struct utsname, nodename} p)
        <*> peekCString (#{ptr struct utsname, release} p)
        <*> peekCString (#{ptr struct utsname, version} p)
        <*> peekCString (#{ptr struct utsname, machine} p)
    poke _ _ = error "There is no reason we should EVER poke a struct utsname. Please check your code."

-- | Get a UName structure for the current kernel
uname :: IO UName
uname = alloca $ \ptr -> do
    c_uname ptr
    peek ptr
