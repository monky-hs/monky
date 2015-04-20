module Battery (getBatteryHandle, getCurrentStatus, getCurrentLevel, BatteryHandle, getTimeLeft)
where

import System.Directory
import Utility
import Config
import Data.IORef

-- FilesArray for the files, those have to be handled different so they
-- get their own type for pattern matching
-- Everything needed as state will be carried in the battery handle
data BatteryHandle = BatH FilesArray (IORef Int)

--PowerNow will use: power_now energy_now energy_full
--ChargeNow will use: voltage_now current_now current_avg charge_now charge_full
data FilesArray = PowerNow File File File File |
  ChargeNow File File File File File File deriving(Show)

pnow_path :: String
pnow_path   = "/sys/class/power_supply/BAT0/power_now"
enow_path :: String
enow_path   = "/sys/class/power_supply/BAT0/energy_now"
efull_path :: String
efull_path  = "/sys/class/power_supply/BAT0/energy_full"
vnow_path :: String
vnow_path   = "/sys/class/power_supply/BAT0/voltage_now"
cnow_path :: String
cnow_path   = "/sys/class/power_supply/BAT0/current_now"
cavg_path :: String
cavg_path   = "/sys/class/power_supply/BAT0/current_avg"
chnow_path :: String
chnow_path  = "/sys/class/power_supply/BAT0/charge_now"
chfull_path :: String
chfull_path = "/sys/class/power_supply/BAT0/charge_full"
adp_path :: String
adp_path    = "/sys/class/power_supply/"++ external_power ++"/online"

getCurrentStatusInt :: File -> IO Int
getCurrentStatusInt f = do
  online <- readValue f
  return online


getCurrentStatus :: BatteryHandle -> IO Int
getCurrentStatus (BatH (PowerNow _ _ _ adp) _) =
  getCurrentStatusInt adp

getCurrentStatus (BatH (ChargeNow _ _ _ _ _ adp) _) =
  getCurrentStatusInt adp


getCurrentLevelInt :: File -> File -> IO Int
getCurrentLevelInt n f = do
  now <- readValue n
  full <- readValue f
  return $ now * 100 `div` full


getCurrentLevel :: BatteryHandle -> IO (Int)
getCurrentLevel (BatH (PowerNow _ now full _) _) =
  getCurrentLevelInt now full

getCurrentLevel (BatH (ChargeNow _ _ _ now full _) _) =
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



getTimeLeft :: BatteryHandle -> IO Int
getTimeLeft (BatH (PowerNow pnow enow efull adp) s)= do
  c <- readIORef s
  (t, n) <- getTimeLeftInt enow pnow efull c adp
  writeIORef s n
  return t

getTimeLeft (BatH (ChargeNow _ _ cavg chnow chfull adp) s)= do
  c <- readIORef s
  (t, n) <- getTimeLeftInt chnow cavg chfull c adp
  return t


createPowerNowHandle :: IO BatteryHandle
createPowerNowHandle = do
  power_now <- fopen pnow_path
  energy_now <- fopen enow_path
  energy_full <- fopen efull_path
  adp_online <- fopen adp_path
  ref <- newIORef (0 :: Int)
  return $BatH (PowerNow power_now energy_now energy_full adp_online) ref

createChargeNowHandle :: IO BatteryHandle
createChargeNowHandle = do
  voltage_now <- fopen vnow_path
  current_now <- fopen cnow_path
  current_avg <- fopen cavg_path
  charge_now <- fopen chnow_path
  charge_full <- fopen chfull_path
  adp_online <- fopen adp_path
  ref <- newIORef (0 :: Int)
  return $BatH (ChargeNow voltage_now current_now current_avg charge_now charge_full adp_online) ref

getBatteryHandle :: IO BatteryHandle
getBatteryHandle = do
  exists <- doesFileExist "/sys/class/power_supply/BAT0/power_now"
  if exists
  then createPowerNowHandle
  else createChargeNowHandle
