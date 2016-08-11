{-
    Copyright 2015,2016 Markus Ongyerth, Stephan Guenther

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
Module      : Monky.Examples.Disk
Description : An example module instance for the disk module
Maintainer  : moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Disk
  ( getDiskHandle
  , DiskH
  )
where

import Formatting

import Monky.Examples.Utility
import Monky.Examples.Images
import Monky.Modules
import Monky.Disk hiding (getDiskHandle)
import qualified Monky.Disk as D (getDiskHandle)

-- |The handle type for this module
newtype DiskH = DH DiskHandle

-- |Get a disk handle
getDiskHandle
  :: String -- ^The UUID of the device to monitor. It has to be mounted at monky startup!
  -> IO DiskH
getDiskHandle = fmap DH . D.getDiskHandle

{- Disk module -}
instance PollModule DiskH where
  getOutput (DH dh) = do
    (dr, dw) <- getDiskReadWrite dh
    df <- getDiskFree dh
    return
      [ diskImage
      , MonkyPlain $ sformat (stext % " " % stext % " " % stext) (convertUnitSI df "B") (convertUnitSI dr "B" ) (convertUnitSI dw "B")
      ]
