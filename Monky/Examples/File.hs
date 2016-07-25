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
{-|
Module      : Monky.Examples.File
Description : Display the first line in a file each tick
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.File
  ( FileHandle
  , getFileHandle
  )
where

import Monky.Modules
import Monky.Utility (fopen, readLine, File)

import qualified Data.Text.Encoding as T

-- |The handle type for this module
newtype FileHandle = FH File

getFile :: FileHandle -> File
getFile (FH f) = f

-- |Get the file handle
getFileHandle
  :: String -- ^Path to the file
  -> IO FileHandle
getFileHandle = fmap FH . fopen

instance PollModule FileHandle where
  getOutput h = do
    line <- readLine . getFile $ h
    return [MonkyPlain $ T.decodeUtf8 line]

-- TODO EvtModule with inotify?
