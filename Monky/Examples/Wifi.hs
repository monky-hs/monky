{-
    Copyright 2016,2017 Markus Ongyerth

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
Module      : Monky.Examples.Wifi
Description : Backwards compatability module for "Monky.Examples.Wifi.Event"
Maintainer  : ongy
Stability   : experimental
Portability : Linux

-}
module Monky.Examples.Wifi
  ( getWifiHandle
  , getWifiHandle'

  , WifiEvtHandle
  , WifiFormat(..)
  )
where

import Monky.Examples.Wifi.Event

