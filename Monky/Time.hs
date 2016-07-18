{-
    Copyright 2015 Markus Ongyerth, Stephan Guenther

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
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.Time
Description : Allows access to read system time
Maintainer  : ongy
Stability   : testing
Portability : Linux
-}

module Monky.Time
  ( TimeHandle
  , getTime
  , getHM
  , getTimeHandle
  )
where

import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime

import System.Locale.Current

-- 4.8 breaks with System.Locale import
#if MIN_VERSION_base(4,8,0)
#else
import System.Locale
#endif

-- |The handle exported by this module.
data TimeHandle = TimeH TimeLocale String TimeZone

doTime :: TimeLocale -> String -> TimeZone -> UTCTime -> String
doTime tl str z t = formatTime tl str $ utcToLocalTime z t

-- |Get the current time in the format given to the handle.
getTime :: TimeHandle -> IO String
getTime (TimeH tl str z) = do
  t <- getCurrentTime
  return $ doTime tl str z t

-- |Get the current time (HH:MM) format for the current time zone.
getHM :: TimeHandle -> IO (Int, Int)
getHM (TimeH _ _ z) = do
  t <- getCurrentTime
  let (LocalTime _ (TimeOfDay h m _)) = utcToLocalTime z t
  return (h, m)

-- |Get a handle for this module
getTimeHandle :: String  -- ^The format that should be used for 'getTime' in strftime format
              -> IO TimeHandle
getTimeHandle format = do
  z <- getCurrentTimeZone
  tl <- currentLocale
  return (TimeH tl format z)
