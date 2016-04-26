{-
    Copyright 2016 Markus Ongyerth

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
Module : Monky.Prepend
Description: Prepend something to a module
Maintainer: ongy
Stability: testing
Portability: linux

use with: packPrepend <Prep> instead of normal pack
-}

module Monky.Prepend
  ( PrepHandle
  , packPrepend
  , PostHandle
  , packAppend
  )
where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Monky.Modules

-- |The handle used by this module, contains underlying module and string
data PrepHandle = Prep String Modules

data PostHandle = Post String Modules

instance Module PrepHandle where
  getText u (Prep x (MW a _)) = (++) x <$> getText u a
  getFDs (Prep _ (MW a _)) = getFDs a
  getEventText fd u (Prep x (MW a _)) = (++)x <$> getEventText fd u a
  setupModule (Prep _ (MW a _)) = setupModule a
  getTextFailable u (Prep x (MW a _)) = do
    ret <- getTextFailable u a
    return ((++) x <$> ret)
  getEventTextFailable fd u (Prep x (MW a _)) = do
    ret <- getEventTextFailable fd u a
    return ((++) x <$> ret)
  recoverModule (Prep _ (MW a _)) = recoverModule a

instance Module PostHandle where
  getText u (Post x (MW a _)) = (x ++) <$> getText u a
  getFDs (Post _ (MW a _)) = getFDs a
  getEventText fd u (Post x (MW a _)) = (x ++) <$> getEventText fd u a
  setupModule (Post _ (MW a _)) = setupModule a
  getTextFailable u (Post x (MW a _)) = do
    ret <- getTextFailable u a
    return ((x ++) <$> ret)
  getEventTextFailable fd u (Post x (MW a _)) = do
    ret <- getEventTextFailable fd u a
    return ((x ++) <$> ret)
  recoverModule (Post _ (MW a _)) = recoverModule a

{-| Create a module that should be prepended with some string

This allows you to prepend an instance of a module with a fixed
String.

For usage look at 'pack'.
-}
packPrepend :: Module a
            => String -- ^The String to prepend
            -> Int -- ^The refresh rate for this module
            -> IO a -- ^The function to get the module
            -> IO Modules -- ^The returned handle
packPrepend x i m = pack i (Prep x <$> pack i m)

{-| Create a module that should be appended with some string

This allows you to append an instance of a module with a fixed
String.

For usage look at 'pack'.
-}
packAppend :: Module a
           => String -- ^The String to prepend
           -> Int -- ^The refresh rate for this module
           -> IO a -- ^The function to get the module
           -> IO Modules -- ^The returned handle
packAppend x i m = pack i (Post x <$> pack i m)
