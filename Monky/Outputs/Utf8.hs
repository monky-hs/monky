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
Module      : Monky.Outputs.Utf8
Description : Output module for utf8
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides the output generation for utf8 outputs
-}
module Monky.Outputs.Utf8
  ( Utf8Output
  , getUtf8Out
  , getUtf8Out'
  )
where

import System.IO (hFlush, stdout)
import Monky.Modules
import Monky.Outputs.Unicode

import qualified Data.Text.IO as T

-- |The output handle for a utf8 pipe
data Utf8Output = Utf8Output MonkyOut

doOut :: MonkyOut -> IO ()
doOut (MonkyPlain t)   = T.putStr t
doOut (MonkyImage _ c) = putChar c -- Images are not supported :(
doOut (MonkyBar p)     = putChar (barChar p)
doOut (MonkyHBar p)    = do
  putStr $ replicate (p `div` 10) '█'
  putChar $ hBarChar (p `mod` 10 * 10)
doOut (MonkyColor _ o) = doOut o

doSegment :: [MonkyOut] -> IO ()
doSegment = mapM_ doOut

instance MonkyOutput Utf8Output where
  doLine _ [] = error "Why are you calling doLine without any modules? I don't think your config makes sense"
  doLine _ [x] = do
    doSegment x
    putStr "\n"
    hFlush stdout
  doLine h@(Utf8Output d) (x:xs) = do
    doSegment x
    doOut d
    doLine h xs

-- |Get an output handle for utf8 formatting. Divider defaults to " | "
getUtf8Out :: IO Utf8Output
getUtf8Out = getUtf8Out' $ MonkyPlain " | "


-- |Get an output handle for utf8 formatting.
getUtf8Out'
  :: MonkyOut -- ^The divider between segments
  -> IO Utf8Output
getUtf8Out' = return . Utf8Output
