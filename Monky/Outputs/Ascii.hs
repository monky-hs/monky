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
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Outputs.Ascii
Description : Output module for Ascii
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides the output generation for ascii outputs
-}
module Monky.Outputs.Ascii
  ( AsciiOutput
  , getAsciiOut
  , getAsciiOut'
  )
where

import System.IO (hFlush, stdout)
import Monky.Modules

import qualified Data.Text.IO as T

-- |The output handle for a ascii pipe
data AsciiOutput = AsciiOutput MonkyOut

doOut :: MonkyOut -> IO ()
doOut (MonkyPlain t)   = T.putStr t
doOut (MonkyImage _ _) = return () -- Images are not supported :(
doOut (MonkyBar p)     = putStr (show p) >> putChar '%'
doOut (MonkyHBar p)    = putStr $ show p
doOut (MonkyColor _ o) = doOut o

doSegment :: [MonkyOut] -> IO ()
doSegment = mapM_ doOut

instance MonkyOutput AsciiOutput where
  doLine _ [] = error "Why are you calling doLine without any modules? I don't think your config makes sense"
  doLine _ [x] = do
    doSegment x
    putStr "\n"
    hFlush stdout
  doLine h@(AsciiOutput d) (x:xs) = do
    doSegment x
    doOut d
    doLine h xs

-- |Get an output handle for ascii formatting. Divider Defaults to @" | "@
getAsciiOut :: IO AsciiOutput
getAsciiOut = getAsciiOut' $ MonkyPlain " | "

-- |Get an output handle for ascii formatting
getAsciiOut'
  :: MonkyOut -- ^The Divider
  -> IO AsciiOutput
getAsciiOut' = return . AsciiOutput
