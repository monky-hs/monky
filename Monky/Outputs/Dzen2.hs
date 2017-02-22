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
Module      : Monky.Outputs/Dzen2
Description : Output module for dzen2
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides the output generation for piping into a dzen2 bar
-}
module Monky.Outputs.Dzen2
  ( DzenOutput
  , getDzenOut
  , getDzenOut'
  , getDzenOut''
  )
where

import System.Directory(getCurrentDirectory)
import System.IO (hFlush, stdout)
import Monky.Modules

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- |The output handle for dzen2 pipe
data DzenOutput = DzenOutput Int Text MonkyOut

doOut :: DzenOutput -> MonkyOut -> IO ()
doOut _ (MonkyPlain t) = T.putStr t
doOut (DzenOutput _ p _) (MonkyImage path _) = do
  T.putStr ("^i(" `T.append` p)
  T.putStr path
  T.putStr ".xbm) "
doOut (DzenOutput h _ _) (MonkyBar p) = do
  T.putStr "^p(3)^p(_TOP)^r(6x"
  putStr . show $ h - div (h * p) 100
  T.putStr ")^pa()"
doOut (DzenOutput h _ _) (MonkyHBar p) = do
  T.putStr "^r("
  putStr . show $ p
  T.putStr ("x" `T.append` (T.pack . show $ h `div` 2) `T.append` ")")
-- Reverse colours for HBar to support the way we draw them
doOut h (MonkyColor (f, b) (MonkyBar p)) = do
  T.putStr ("^bg(" `T.append` f `T.append` ")^fg(" `T.append` b `T.append` ")")
  doOut h (MonkyBar p)
  T.putStr "^bg()^fg()"
doOut h (MonkyColor (f, b) o) = do
  T.putStr ("^bg(" `T.append` b `T.append` ")^fg(" `T.append` f `T.append` ")")
  doOut h o
  T.putStr "^bg()^fg()"

doSegment :: DzenOutput -> [MonkyOut] -> IO ()
doSegment h = mapM_ (doOut h)

instance MonkyOutput DzenOutput where
  doLine _ [] = error "Why are you calling doLine without any modules? I don't think your config makes sense"
  doLine h [x] = do
    doSegment h x
    putStr "\n"
    hFlush stdout
  doLine h@(DzenOutput _ _ d) (x:xs) = do
    doSegment h x
    doOut h d
    doLine h xs

-- |Get an output handle for dzen2 formatting
-- Assumes @" | "@ as divider
getDzenOut
  :: Int -- ^The height of your dzen bar in pixel (required for block-drawing)
  -> Text -- ^Path to the directory cointaining your .xbm files.
  -> IO DzenOutput
getDzenOut h p = return $ DzenOutput h (T.append p "/") $ MonkyPlain " | "


-- |Get the output handle for dzen2 formatting. Will asume your .xbm files are
-- in \<monkydir\>\/xbm\/
getDzenOut'
    :: Int -- ^The height of your dzen bar in pixel (for block drawing)
    -> IO DzenOutput
getDzenOut' h = do
    pwd <- getCurrentDirectory
    getDzenOut h (T.pack pwd `T.append` "/xbm/")

-- |Get an output handle for dzen2 formatting
getDzenOut''
    :: Int -- ^The height of your dzen bar in pixel (required for block-drawing)
    -> Text -- ^Path to the directory containing your .xbm files.
    -> MonkyOut -- ^Divider
    -> IO DzenOutput
getDzenOut'' h p d = return $ DzenOutput h (T.append p "/") d
