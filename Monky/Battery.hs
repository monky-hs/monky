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
Module      : Monky.Battery
Description : Allows access to information about a battery connected to the system
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module allows to read information about a battery connected to the system.

This module does not support every setup yet.
The files in /sys/ used by this module are driver dependend.
If it crashes monky for you setup please make a bug report with an
`ls /sys/class/power_supply/" ++ e ++ "/` attached.
-}

module Monky.Battery
  ( getBatteryHandle
  , getBatteryHandle'
  , getCurrentStatus
  , getCurrentLevel
  , BatteryHandle
  , getTimeLeft
  , getLoading
  , BatteryState(..)
  )
where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Data.Maybe (fromMaybe)
import Data.IORef
import System.Directory
import Monky.Utility

-- FilesArray for the files, those have to be handled different so they
-- get their own type for pattern matching
-- Everything needed as state will be carried in the battery handle
-- |The handle exported by this module
data BatteryHandle = BatH FilesArray (IORef Int)

--PowerNow will use: power_now energy_now energy_full
--ChargeNow will use: voltage_now current_now current_avg charge_now charge_full
data FilesArray = PowerNow File File File File |
  ChargeNow File File (Maybe File) File File File deriving(Show)

-- |Datatype to represent battery state
data BatteryState = BatFull | BatLoading | BatDraining

pnowPath :: String -> String
pnowPath e  = "/sys/class/power_supply/" ++ e ++ "/power_now"
enowPath :: String -> String
enowPath e  = "/sys/class/power_supply/" ++ e ++ "/energy_now"
efullPath :: String -> String
efullPath e = "/sys/class/power_supply/" ++ e ++ "/energy_full"
vnowPath :: String -> String
vnowPath e  = "/sys/class/power_supply/" ++ e ++ "/voltage_now"
cnowPath :: String -> String
cnowPath e  = "/sys/class/power_supply/" ++ e ++ "/current_now"
cavgPath :: String -> String
cavgPath e  = "/sys/class/power_supply/" ++ e ++ "/current_avg"
chnowPath :: String -> String
chnowPath e = "/sys/class/power_supply/" ++ e ++ "/charge_now"
chfullPath :: String -> String
chfullPath e = "/sys/class/power_supply/" ++ e ++ "/charge_full"
adpPath :: String -> String
adpPath e  = "/sys/class/power_supply/"++ e ++"/online"

-- |Internal function for getCurrentStatus
getCurrentStatusInt :: File -> IO Int
getCurrentStatusInt = readValue


getCurrentStatusM :: BatteryHandle -> IO Int
getCurrentStatusM (BatH (PowerNow _ _ _ adp) _) =
  getCurrentStatusInt adp
getCurrentStatusM (BatH (ChargeNow _ _ _ _ _ adp) _) =
  getCurrentStatusInt adp

-- |Get the current state of the battery (loading/draining/full)
getCurrentStatus :: BatteryHandle -> IO BatteryState
getCurrentStatus h = do
  val <- getCurrentStatusM h
  case val of
    1 -> return BatLoading
    0 -> return BatDraining
    _ -> return BatFull

-- |Internal function for getcurrentLevel
getCurrentLevelInt :: File -> File -> IO Int
getCurrentLevelInt n f = do
  now <- readValue n
  full <- readValue f
  return $ now * 100 `div` full

-- |Get the charge left in the battery in percent.
getCurrentLevel :: BatteryHandle -> IO Int
getCurrentLevel (BatH (PowerNow _ now full _) _) =
  getCurrentLevelInt now full
getCurrentLevel (BatH (ChargeNow _ _ _ now full _) _) =
  getCurrentLevelInt now full


-- |Internal function for getTimeLeft
getTimeLeftInt :: File -> File -> File -> Int -> File -> IO (Int, Int)
getTimeLeftInt n c f s adp = do
  online <- getCurrentStatusInt adp
  now <- readValue n
  full <- readValue f
  let gap = if online == 0 then now else full - now
  change <- readValue c
  let avg = (change * 20 + s * 80) `div` 100
  return (sdivUBound gap (avg `div` 3600) 0, avg)

-- |Get current loading speed in Watt/s
getLoading :: BatteryHandle -> IO Float
getLoading (BatH (PowerNow pnow _ _ _) _) = do
  power <- readLine pnow
  return $(read power :: Float) / 1000000
getLoading (BatH (ChargeNow vnow cnow _ _ _ _) _) = do
  voltage <- readLine vnow
  current <- readLine cnow
  let pow = ((read voltage :: Float) * (read current :: Float)) / 1000000000000
  return pow

-- |Get an approximated amount of seconds left until the battery runs out
getTimeLeft :: BatteryHandle -> IO Int
getTimeLeft (BatH (PowerNow pnow enow efull adp) s)= do
  c <- readIORef s
  (t, n) <- getTimeLeftInt enow pnow efull c adp
  writeIORef s n
  return t
getTimeLeft (BatH (ChargeNow _ cnow cavg chnow chfull adp) s)= do
  c <- readIORef s
  (t, _) <- getTimeLeftInt chnow (fromMaybe cnow cavg) chfull c adp
  return $ (floor . sqrt $(fromIntegral t :: Float)) * 60

-- |Create a power handle that uses the power_now file
createPowerNowHandle :: String -> String -> IO BatteryHandle
createPowerNowHandle e b = do
  power_now <- fopen $ pnowPath b
  energy_now <- fopen $ enowPath b
  energy_full <- fopen $ efullPath b
  adp_online <- fopen $ adpPath e
  ref <- newIORef (0 :: Int)
  return $BatH (PowerNow power_now energy_now energy_full adp_online) ref

-- |Create a power handle that uses the charge_now file
createChargeNowHandle :: String -> String -> IO BatteryHandle
createChargeNowHandle e b = do
  voltage_now <- fopen $ vnowPath b
  current_now <- fopen $ cnowPath b
  exists <- doesFileExist $ cavgPath b
  current_avg <- if exists then Just <$> fopen (cavgPath b) else return Nothing
  charge_now <- fopen $ chnowPath b
  charge_full <- fopen $ chfullPath b
  adp_online <- fopen $ adpPath e
  ref <- newIORef (0 :: Int)
  return $BatH (ChargeNow voltage_now current_now current_avg charge_now charge_full adp_online) ref

-- |Create a 'BatteryHandle'
getBatteryHandle :: String  -- ^The name of the wall socket adapter used by the battery
                 -> String -- ^The name of the battery
                 -> IO BatteryHandle
getBatteryHandle e b = do
  exists <- doesFileExist $ pnowPath b
  if exists
    then createPowerNowHandle e b
    else createChargeNowHandle e b

-- |Version which defaults to "BAT0"
getBatteryHandle' :: String -> IO BatteryHandle
getBatteryHandle' = flip getBatteryHandle "BAT0"
