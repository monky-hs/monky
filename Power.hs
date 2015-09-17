{-|
Description : delete me
TODO remove
-}
module Power (PowerHandle, getPowerHandle, getPowerNow)
where

import Utility
import System.Directory

data PowerHandle = PowerH File File | PwH File

pathVoltage :: String
pathVoltage = "/sys/class/power_supply/BAT0/voltage_now"

pathCurrent :: String
pathCurrent = "/sys/class/power_supply/BAT0/current_now"

pathPower :: String
pathPower = "/sys/class/power_supply/BAT0/power_now"

getPowerNow :: PowerHandle -> IO Float
getPowerNow (PowerH v c) = do
  voltage <- readLine v
  current <- readLine c
  let pow = ((read voltage :: Float) * (read current :: Float)) / 1000000000000
  return pow

getPowerNow (PwH p) = do
  power <- readLine p
  let pow = (read power :: Float) / 1000000
  return pow

getPowerHandle :: IO PowerHandle
getPowerHandle = do
  exists <- doesFileExist "/sys/class/power_supply/BAT0/power_now"
  if exists
  then do file_power <- fopen pathPower
          return $PwH file_power
  else do file_voltage <- fopen pathVoltage
          file_current <- fopen pathCurrent
          return $PowerH file_voltage file_current
