module Battery (getBatteryHandle, getCurrentStatus, getCurrentLevel, BatteryHandle, getTimeLeft)
where

import Control.Monad
import System.Directory
import Utility

-- FilesArray for the files, those have to be handled different so they
-- get their own type for pattern matching
-- Everything needed as state will be carried in the battery handle
data BatteryHandle = BatH FilesArray Int

--PowerNow will use: power_now energy_now energy_full
--ChargeNow will use: voltage_now current_now current_avg charge_now charge_full
data FilesArray = PowerNow File File File File |
  ChargeNow File File File File File File deriving(Show)


pnow_path   = "/sys/class/power_supply/BAT0/power_now"
enow_path   = "/sys/class/power_supply/BAT0/energy_now"
efull_path  = "/sys/class/power_supply/BAT0/energy_full"
vnow_path   = "/sys/class/power_supply/BAT0/voltage_now"
cnow_path   = "/sys/class/power_supply/BAT0/current_now"
cavg_path   = "/sys/class/power_supply/BAT0/current_avg"
chnow_path  = "/sys/class/power_supply/BAT0/charge_now"
chfull_path = "/sys/class/power_supply/BAT0/charge_full"
adp_path    = "/sys/class/power_supply/ADP1/online"


getCurrentStatusInt :: File -> IO Int
getCurrentStatusInt f = do
  online <- readValue f
  return online


getCurrentStatus :: BatteryHandle -> IO (Int, BatteryHandle)
getCurrentStatus (BatH (PowerNow f1 now full adp) s) =
  liftM (flip (,) (BatH (PowerNow f1 now full adp) s))
  $ getCurrentStatusInt adp

getCurrentStatus (BatH (ChargeNow f1 f2 f3 now full adp) s) =
  liftM (flip (,) (BatH (ChargeNow f1 f2 f3 now full adp) s))
  $ getCurrentStatusInt adp


getCurrentLevelInt :: File -> File -> IO Int
getCurrentLevelInt n f = do
  now <- readValue n
  full <- readValue f
  return $ now * 100 `div` full


getCurrentLevel :: BatteryHandle -> IO (Int, BatteryHandle)
getCurrentLevel (BatH (PowerNow f1 now full adp) s) =
  liftM (flip (,) (BatH (PowerNow f1 now full adp) s))
  $ getCurrentLevelInt now full

getCurrentLevel (BatH (ChargeNow f1 f2 f3 now full adp) s) =
  liftM (flip (,) (BatH (ChargeNow f1 f2 f3 now full adp) s))
  $ getCurrentLevelInt now full


getTimeLeftInt :: File -> File -> Int -> IO (Int, Int)
getTimeLeftInt n c s = do
  now <- readValue n
  new <- readValue c
  let avg = (new*20+s*80) `div` 100
  return ( if avg >= 3600 then now `div` (avg `div` 3600) else 0, avg)



getTimeLeft :: BatteryHandle -> IO (Int, BatteryHandle)
getTimeLeft (BatH (PowerNow pnow enow f adp) s)= do
  (t, n) <- getTimeLeftInt enow pnow s
  return (t, BatH (PowerNow pnow enow f adp) n)

getTimeLeft (BatH (ChargeNow f1 f2 cavg chnow f3 adp) s)= do
  (t, n) <- getTimeLeftInt chnow cavg s
  return (t, BatH (ChargeNow f1 f2 cavg chnow f3 adp) n)


createPowerNowHandle :: IO BatteryHandle
createPowerNowHandle = do
  power_now <- fopen pnow_path
  energy_now <- fopen enow_path
  energy_full <- fopen efull_path
  adp_online <- fopen adp_path
  return $BatH (PowerNow power_now energy_now energy_full adp_online) 0

createChargeNowHandle :: IO BatteryHandle
createChargeNowHandle = do
  voltage_now <- fopen vnow_path
  current_now <- fopen cnow_path
  current_avg <- fopen cavg_path
  charge_now <- fopen chnow_path
  charge_full <- fopen chfull_path
  adp_online <- fopen adp_path
  return $BatH (ChargeNow voltage_now current_now current_avg charge_now charge_full adp_online) 0

getBatteryHandle :: IO BatteryHandle
getBatteryHandle = do
  exists <- doesFileExist "/sys/class/power_supply/BAT0/power_now"
  if exists
  then createPowerNowHandle
  else createChargeNowHandle
