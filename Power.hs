module Power (PowerHandle, getPowerHandle, getPowerNow)
where

import Utility

data PowerHandle = PowerH File File

path_voltage :: String
path_voltage = "/sys/class/power_supply/BAT0/voltage_now"

path_current :: String
path_current = "/sys/class/power_supply/BAT0/current_now"

getPowerNow :: PowerHandle -> IO (Float, PowerHandle)
getPowerNow (PowerH v c) = do
  voltage <- readLine v
  current <- readLine c
  let pow = ((read voltage :: Float) * (read current :: Float)) / 1000000000000
  return (pow, PowerH v c)

getPowerHandle :: IO PowerHandle
getPowerHandle = do
  file_voltage <- fopen path_voltage
  file_current <- fopen path_current
  return $PowerH file_voltage file_current
