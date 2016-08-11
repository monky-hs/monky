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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.Outputs.Fallback
Description : Output module for doing a "best guess"
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Outputs.Fallback
  ( WrapOuts
  , getFallbackOut
  , chooseTerminalOut
  )
where

import System.IO
import GHC.IO.Encoding
import Monky.Modules

import Monky.Outputs.Ascii
import Monky.Outputs.Utf8

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

-- |The datatype for wrapping outher outputs
data WrapOuts = forall a . MonkyOutput a => WO a

instance MonkyOutput WrapOuts where
  doLine (WO o) = doLine o


-- |Lowerlevel access to guess which terminal out to use based on system encoding
chooseTerminalOut :: IO WrapOuts
chooseTerminalOut = do
  l <- getLocaleEncoding
  if textEncodingName l == "UTF-8"
    then WO <$> getUtf8Out
    else WO <$> getAsciiOut

{- | Wrapper for normal outputs that tries to find the best output

This function will check if stdout is a terminal and switch to AsciiOut or UTf8Out depending on the locale
-}
getFallbackOut
  :: MonkyOutput a
  => IO a -- ^The output to use for non-terminal mode
  -> IO WrapOuts
getFallbackOut o = do
  e <- hIsTerminalDevice stdout
  if e
    then chooseTerminalOut
    else WO <$> o
