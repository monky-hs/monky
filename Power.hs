module Power (PowerHandle, getPowerHandle, getPowerNow)
where

import Utility
import System.Directory

data PowerHandle = PowerH File File | PwH File

path_voltage :: String
path_voltage = "/sys/class/power_supply/BAT0/voltage_now"

path_current :: String
path_current = "/sys/class/power_supply/BAT0/current_now"

path_power :: String
path_power = "/sys/class/power_supply/BAT0/power_now"

getPowerNow :: PowerHandle -> IO Float
getPowerNow (PowerH v c) = do
  voltage <- readLine v
  current <- readLine c
  let pow = ((read voltage :: Float) * (read current :: Float)) / 1000000000000
  return pow

getPowerNow (PwH p) = do
  power <- readLine p
  let pow = ((read power :: Float) / 1000000)
  return pow

getPowerHandle :: IO PowerHandle
getPowerHandle = do
  exists <- doesFileExist "/sys/class/power_supply/BAT0/power_now"
  if exists
  then do file_power <- fopen path_power
          return $PwH file_power
  else do file_voltage <- fopen path_voltage
          file_current <- fopen path_current
          return $PowerH file_voltage file_current
