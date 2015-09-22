{-# LANGUAGE CPP #-}
{-|
Module      : Monky.Time
Description : Allows access to read system time
Maintainer  : ongy
Stability   : testing
Portability : Linux
-}

module Monky.Time
(TimeHandle, getTime, getHM, getTimeHandle)
where

import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime

-- 4.8 breaks with System.Locale import
#if MIN_VERSION_base(4,8,0)
#else
import System.Locale
#endif

-- |The handle exported by this module.
data TimeHandle = TimeH String


-- |Get the current time in the format given to the handle.
getTime :: TimeHandle -> IO String
getTime (TimeH str) = do
  t <- getCurrentTime
  z <- getCurrentTimeZone
  return $formatTime defaultTimeLocale str $utcToLocalTime z t

-- |Get the current time (HH:MM) format for the current time zone.
getHM :: TimeHandle -> IO (Int, Int)
getHM _ = do
  t <- getCurrentTime
  z <- getCurrentTimeZone
  let (LocalTime _ (TimeOfDay h m _)) = utcToLocalTime z t
  return (h, m)

-- |Get a handle for this module
getTimeHandle :: String  -- ^The format that should be used for 'getTime' in strftime format
              -> IO TimeHandle
getTimeHandle format = return (TimeH format)
