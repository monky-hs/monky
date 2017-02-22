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
Module      : Monky.Outputs.I3
Description : Output module for i3-bar
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides the output generation for i3-bar outputs.
Do note, if you are using this output, you have to compile monky
once before you are piping it to i3-bar.
GHC will output something during compilation step, this can't be avoided
by monky, this will break the output.
-}
module Monky.Outputs.I3
  ( I3Output
  , getI3Output
  )
where

import Monky.Modules
import Monky.Outputs.Unicode

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (hFlush, stdout)

-- |Output handle for i3-bar
data I3Output = I3Output

i3Full :: Text -> Text
i3Full xs = "\"full_text\": \"" `T.append` xs `T.append` "\""

makeColor :: Text -> Text -> Text
makeColor "" _ = ""
makeColor x y = T.concat [ ", \"", y, "\": \"", x, "\"" ]

getOut :: MonkyOut -> Text
getOut (MonkyPlain t)   = i3Full t
getOut (MonkyImage _ c)   = i3Full $ T.singleton c -- Images are not supported :(
getOut (MonkyBar p)     = i3Full $ T.singleton (barChar p)
getOut (MonkyHBar p)    = i3Full $ T.pack (replicate (p `div` 10) '█') `T.append` (T.singleton $ hBarChar (p `mod` 10 * 10))
getOut (MonkyColor (f, b) o) = T.concat
  [ getOut o
  , makeColor f "color"
  , makeColor b "background"
  ]

doSegment :: [MonkyOut] -> IO ()
doSegment [] = return ()
doSegment [x] = do
  putChar '{'
  T.putStr $ getOut x
  putChar '}'
doSegment (x:xs) = do
  putChar '{'
  T.putStr $ getOut x
  putStr ", \"separator\": false"
  putStr ", \"separator_block_width\": 1"
  putStr "},"
  doSegment xs

outputLine :: [[MonkyOut]] -> IO ()
outputLine [] = error "i3-output outputLIne should never be called with an empty list"
outputLine [x] = doSegment x
outputLine (x:xs) = do
  doSegment x
  unless (null x) (putChar ',')
  outputLine xs

instance MonkyOutput I3Output where
  doLine _ xs = do
    putChar '['
    outputLine xs
    putStr "],"
    putChar '\n'
    hFlush stdout


-- | Get output handle for i3-bar. This initializes the communication on generation
getI3Output :: IO I3Output
getI3Output = do
  putStrLn "{\"version\":1}" -- Static version thingy we have to print
  putChar '[' -- Start the output lines array
  return I3Output
