module CPU (CPUHandle, getCPUHandle, getCPUPercent, getCPUTemp)
where

import Utility
import System.IO
import Data.List

data CPUHandle = CPUH [Int] [Int]

pathStat :: String
pathStat = "/proc/stat"

pathTemp :: String
pathTemp = "/sys/class/thermal/thermal_zone0/temp"

getCPUPercent :: CPUHandle -> IO ([Int], CPUHandle)
getCPUPercent (CPUH a w) = do
  content <- readFile pathStat
  let d = map (map read) (map (drop 1) (map words (filter (\l -> isPrefixOf "cpu"l && ((>3) . length . head . words $ l)) (lines content)))) :: [[Int]]
  let all = map sum d
  let work = map sum (map (take 3) d)
  let cwork = zipWith (-) work w
  let call = zipWith (-) all a
  return (zipWith div (map (* 100) cwork) call, (CPUH all work))

getCPUTemp :: CPUHandle -> IO (Int, CPUHandle)
getCPUTemp cpuh = do
  temp <- readFile pathTemp
  return (div (read temp :: Int) 1000, cpuh)

getCPUHandle :: IO CPUHandle
getCPUHandle = do
  return $CPUH [0] [0]
