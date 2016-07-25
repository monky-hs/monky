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
{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : Monky.Examples.Connectivity
Description : Simple Connectivity example
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Connectivity
  ( getConnH
  , Conn
  )
where

import Data.Composition ((.:))
import Data.Text (Text)
import Monky.Modules
import Monky.Connectivity hiding (getConnH)
import qualified Monky.Connectivity as C (getConnH)

showCon :: Bool -> Text
showCon False = "Unconnected"
showCon True  = "  Connected"

-- |The handle type for this module
newtype Conn = Conn ConnHandle

-- |Get a handle that allows testing for connectivity to a server
getConnH
  :: String -- ^The Host to use for connectivity probing
  -> Int -- ^Which port to use for connecivity probing (drop is bad)
  -> IO Conn
getConnH = fmap Conn .: C.getConnH

instance PollModule Conn where
  getOutput (Conn h) = fmap (\e -> [MonkyPlain $ showCon e]) $ hasConn h
