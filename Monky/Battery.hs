{-|
Module      : Battery
Description : Allows acces to information about a battery connected to the system
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module allows to read bits of information about a battery connected to tye
system
The module is initialized by creating a 'BatteryHandle'.
A 'PowerHandle' is required for some functions to work properly.
-}
module Monky.Battery
(getBatteryHandle, getCurrentStatus, getCurrentLevel, BatteryHandle, getTimeLeft, getLoading)
where

import Monky.Config
import Data.IORef
import System.Directory
import Monky.Utility

-- FilesArray for the files, those have to be handled different so they
-- get their own type for pattern matching
-- Everything needed as state will be carried in the battery handle
{- |A handle used to access information about the battery

  This has to be given as argument to all functions of this module except the
  getBatteryHandle
-} 
data BatteryHandle = BatH FilesArray (IORef Int)

--PowerNow will use: power_now energy_now energy_full
--ChargeNow will use: voltage_now current_now current_avg charge_now charge_full
data FilesArray = PowerNow File File File File |
  ChargeNow File File File File File File deriving(Show)

pnowPath :: String
pnowPath   = "/sys/class/power_supply/BAT0/power_now"
enowPath :: String
enowPath   = "/sys/class/power_supply/BAT0/energy_now"
efullPath :: String
efullPath  = "/sys/class/power_supply/BAT0/energy_full"
vnowPath :: String
vnowPath   = "/sys/class/power_supply/BAT0/voltage_now"
cnowPath :: String
cnowPath   = "/sys/class/power_supply/BAT0/current_now"
cavgPath :: String
cavgPath   = "/sys/class/power_supply/BAT0/current_avg"
chnowPath :: String
chnowPath  = "/sys/class/power_supply/BAT0/charge_now"
chfullPath :: String
chfullPath = "/sys/class/power_supply/BAT0/charge_full"
adpPath :: String
adpPath    = "/sys/class/power_supply/"++ external_power ++"/online"

-- |Internal function for getCurrentStatus
getCurrentStatusInt :: File -> IO Int
getCurrentStatusInt = readValue

-- |TODO
getCurrentStatus :: BatteryHandle -> IO Int
getCurrentStatus (BatH (PowerNow _ _ _ adp) _) =
  getCurrentStatusInt adp
getCurrentStatus (BatH (ChargeNow _ _ _ _ _ adp) _) =
  getCurrentStatusInt adp

-- |Internal function for getcurrentLevel
getCurrentLevelInt :: File -> File -> IO Int
getCurrentLevelInt n f = do
  now <- readValue n
  full <- readValue f
  return $ now * 100 `div` full

-- |Get the current power level in percent
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
  let avg = (change*20+s*80) `div` 100
  return ( if avg >= 3600 then gap `div` (avg `div` 3600) else 0, avg)

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
getTimeLeft (BatH (ChargeNow _ _ cavg chnow chfull adp) s)= do
  c <- readIORef s
  (t, _) <- getTimeLeftInt chnow cavg chfull c adp
  return t


-- |Create a power handle that uses the power_now file
createPowerNowHandle :: IO BatteryHandle
createPowerNowHandle = do
  power_now <- fopen pnowPath
  energy_now <- fopen enowPath
  energy_full <- fopen efullPath
  adp_online <- fopen adpPath
  ref <- newIORef (0 :: Int)
  return $BatH (PowerNow power_now energy_now energy_full adp_online) ref

-- |Create a power handle that uses the charge_now file
createChargeNowHandle :: IO BatteryHandle
createChargeNowHandle = do
  voltage_now <- fopen vnowPath
  current_now <- fopen cnowPath
  current_avg <- fopen cavgPath
  charge_now <- fopen chnowPath
  charge_full <- fopen chfullPath
  adp_online <- fopen adpPath
  ref <- newIORef (0 :: Int)
  return $BatH (ChargeNow voltage_now current_now current_avg charge_now charge_full adp_online) ref

-- |Opens the files used for the battery calculations
getBatteryHandle :: IO BatteryHandle
getBatteryHandle = do
  exists <- doesFileExist "/sys/class/power_supply/BAT0/power_now"
  if exists
    then createPowerNowHandle
    else createChargeNowHandle
