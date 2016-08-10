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
{-# LANGUAGE CPP, OverloadedStrings #-}
{-|
Module      : Monky.Examples.Images
Description : Exports 'MonkyImage's that can/should be used.
Maintainer  : ongy
Stability   : testing
Portability : Linux

GHC enforces some rules on unicode strings, which makes images version dependant.
This module exports the appropriate image.
-}
module Monky.Examples.Images
where

import Data.Text (Text)
import Monky.Modules (MonkyOut(MonkyImage))

-- | MonkyImage for disk (primary persistent storage)
diskImage :: MonkyOut
diskImage =
#if MIN_VERSION_base(4,8,0)
  MonkyImage "diskette" '🖪'
#else
  MonkyImage "diskette" 'D'
#endif

-- | MonkyImage for memory (RAM (it's a ram))
memoryImage :: MonkyOut
memoryImage =
  MonkyImage "mem" '🐏'

-- | MonkyImage for time with custom image support
fancyTimeImage :: Text -> MonkyOut
fancyTimeImage = flip MonkyImage '🕐'

-- | MonkyImage for time (clock)
timeImage :: MonkyOut
timeImage = fancyTimeImage "clock"

-- | MonkyImage with custom image support (loadding etc.)
batteryImage :: Text -> MonkyOut
batteryImage = flip MonkyImage '🔋'

-- | MonkyImage to be used for CPU
cpuImage :: MonkyOut
cpuImage =
#if MIN_VERSION_base(4,8,0)
  MonkyImage "cpu" '🖩'
#else
  MonkyImage "cpu" 'C'
#endif
