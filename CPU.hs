module CPU (CPUHandle, getCPUHandle, getCPUPercent)
where

import Utility
import System.IO
import Data.List

data CPUHandle = CPUH [Int] [Int]

getCPUPercent :: CPUHandle -> Int -> IO ([Int], CPUHandle)
getCPUPercent (CPUH a w) _ = do
  content <- readFile "/proc/stat"
  let d = map (map read) (map (drop 1) (map words (filter (\l -> isPrefixOf "cpu"l && ((>3) . length . head .words $ l)) (lines content)))) :: [[Int]]
  let all = map sum d
  let work = map sum (map (take 3) d)
  let cwork = zipWith (-) work w
  let call = zipWith (-) all a
  return (zipWith div (map (* 100) cwork) call, (CPUH all work))

getCPUHandle :: IO CPUHandle
getCPUHandle = do
  return $CPUH [0] [0]
