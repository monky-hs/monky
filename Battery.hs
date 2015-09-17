module Battery (getBatteryHandle, getCurrentStatus, getCurrentLevel, BatteryHandle, getTimeLeft, getLoading)
where

import Power

import Config
import Data.IORef
import System.Directory
import Utility

-- FilesArray for the files, those have to be handled different so they
-- get their own type for pattern matching
-- Everything needed as state will be carried in the battery handle
data BatteryHandle = BatH PowerHandle FilesArray (IORef Int)

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

getCurrentStatusInt :: File -> IO Int
getCurrentStatusInt = readValue

getCurrentStatus :: BatteryHandle -> IO Int
getCurrentStatus (BatH _ (PowerNow _ _ _ adp) _) =
  getCurrentStatusInt adp

getCurrentStatus (BatH _ (ChargeNow _ _ _ _ _ adp) _) =
  getCurrentStatusInt adp


getCurrentLevelInt :: File -> File -> IO Int
getCurrentLevelInt n f = do
  now <- readValue n
  full <- readValue f
  return $ now * 100 `div` full


getCurrentLevel :: BatteryHandle -> IO Int
getCurrentLevel (BatH _ (PowerNow _ now full _) _) =
  getCurrentLevelInt now full

getCurrentLevel (BatH _ (ChargeNow _ _ _ now full _) _) =
  getCurrentLevelInt now full


getTimeLeftInt :: File -> File -> File -> Int -> File -> IO (Int, Int)
getTimeLeftInt n c f s adp = do
  online <- getCurrentStatusInt adp
  now <- readValue n
  full <- readValue f
  let gap = if online == 0 then now else full - now
  change <- readValue c
  let avg = (change*20+s*80) `div` 100
  return ( if avg >= 3600 then gap `div` (avg `div` 3600) else 0, avg)

getLoading :: BatteryHandle -> IO Float
getLoading (BatH ph _ _) = getPowerNow ph



getTimeLeft :: BatteryHandle -> IO Int
getTimeLeft (BatH _ (PowerNow pnow enow efull adp) s)= do
  c <- readIORef s
  (t, n) <- getTimeLeftInt enow pnow efull c adp
  writeIORef s n
  return t

getTimeLeft (BatH _ (ChargeNow _ _ cavg chnow chfull adp) s)= do
  c <- readIORef s
  (t, n) <- getTimeLeftInt chnow cavg chfull c adp
  return t


createPowerNowHandle :: PowerHandle -> IO BatteryHandle
createPowerNowHandle ph = do
  power_now <- fopen pnowPath
  energy_now <- fopen enowPath
  energy_full <- fopen efullPath
  adp_online <- fopen adpPath
  ref <- newIORef (0 :: Int)
  return $BatH ph (PowerNow power_now energy_now energy_full adp_online) ref

createChargeNowHandle :: PowerHandle -> IO BatteryHandle
createChargeNowHandle ph = do
  voltage_now <- fopen vnowPath
  current_now <- fopen cnowPath
  current_avg <- fopen cavgPath
  charge_now <- fopen chnowPath
  charge_full <- fopen chfullPath
  adp_online <- fopen adpPath
  ref <- newIORef (0 :: Int)
  return $BatH ph (ChargeNow voltage_now current_now current_avg charge_now charge_full adp_online) ref

getBatteryHandle :: PowerHandle -> IO BatteryHandle
getBatteryHandle ph = do
  exists <- doesFileExist "/sys/class/power_supply/BAT0/power_now"
  if exists
  then createPowerNowHandle ph
  else createChargeNowHandle ph
