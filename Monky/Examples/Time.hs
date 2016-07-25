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
Module      : Monky.Examples.Time
Description : An example module instance for the time module
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Time
  ( getTimeHandle
  , getFancyTimeHandle

  , THandle
  , FTHandle
  )
where

import Formatting
import qualified Data.Text as T

import Control.Arrow ((***))

import Monky.Modules
import Monky.Time hiding (getTimeHandle)
import qualified Monky.Time as MT (getTimeHandle)

-- |Handle for accessing system time
newtype THandle = TH TimeHandle

-- |Get a 'THandle'
getTimeHandle :: String  -- ^The format that should be used for 'getTime' in strftime format
              -> IO THandle
getTimeHandle = fmap TH . MT.getTimeHandle

{- Time Module -}
instance PollModule THandle where
  getOutput (TH h) = do
    ts <- getTime h
    return
      [ MonkyImage "clock" '🕐'
      , MonkyPlain $ T.pack ts
      ]

{- Time Module -}
timeToXBM :: (Int, Int) -> (Int, Int)
timeToXBM = (`mod` 12) *** (`div` 15)
--timeToXBM (h, m) = (xh, xm)
--  where xh = h `mod` 12
--        xm = m `div` 15

-- |Like 'THandle' but uses a fancy variant of clock images
newtype FTHandle = FTH TimeHandle

-- |Get a 'FTHandle'
getFancyTimeHandle :: String  -- ^The format that should be used for 'getTime' in strftime format
                   -> IO FTHandle
getFancyTimeHandle = fmap FTH . MT.getTimeHandle

instance PollModule FTHandle where
  getOutput (FTH h) = do
    ts <- getTime h
    t <- getHM h
    let (th, tm) = timeToXBM t
    return
      [ MonkyImage (sformat (int % "-" % int) th tm) '🕐'
      , MonkyPlain . T.pack $ ts
      ]

