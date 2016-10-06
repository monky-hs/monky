{-
    Copyright 2015,2016 Markus Ongyerth

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
Module      : Monky.Examples.Alsa
Description : Proxy module for backwards compat. See Monky.Examples.Sound.Alsa
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Alsa {-# DEPRECATED "Use Monky.Examples.Sound.Alsa" #-}
  ( getVOLHandle
  , AlsaH
  )
where

import Monky.Examples.Sound.Alsa
