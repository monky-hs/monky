module CPU (CPUHandle, getCPUHandle, getCPUPercent, getCPUTemp, getCPUMaxScalingFreq)
where

import Utility
import System.IO
import Data.List
import Data.IORef

data CPUHandle = CPUH (IORef [Int]) (IORef [Int])

pathStat :: String
pathStat = "/proc/stat"

pathTemp :: String
pathTemp = "/sys/class/thermal/thermal_zone0/temp"

pathMaxScalingFreq :: String
pathMaxScalingFreq = "/sys/devices/system/cpu/cpu0/cpufreq/scaling_max_freq"

getCPUPercent :: CPUHandle -> IO [Int]
getCPUPercent (CPUH aref wref) = do
  content <- readFile pathStat
  let d = map (map read) (map (drop 1) (map words (filter (\l -> isPrefixOf "cpu"l && ((>3) . length . head . words $ l)) (lines content)))) :: [[Int]]
  let all = map sum d
  let work = map sum (map (take 3) d)
  a <- readIORef aref
  w <- readIORef wref
  let cwork = zipWith (-) work w
  let call = zipWith (-) all a
  writeIORef wref work
  writeIORef aref all
  return $zipWith div (map (* 100) cwork) call

getCPUTemp :: CPUHandle -> IO Int
getCPUTemp cpuh = do
  temp <- readFile pathTemp
  return $div (read temp :: Int) 1000

getCPUMaxScalingFreq :: CPUHandle -> IO Float
getCPUMaxScalingFreq cpuh = do
  freq <- readFile pathMaxScalingFreq
  return $(read freq :: Float) / 1000000

getCPUHandle :: IO CPUHandle
getCPUHandle = do
  workref <- newIORef ([0] :: [Int])
  allref <- newIORef ([0] :: [Int])
  return $CPUH allref workref
