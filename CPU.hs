module CPU (CPUHandle, getCPUHandle, getCPUPercent)
where

import Utility
import System.IO
import Text.Printf
import Data.List

data CPUHandle = CPUH File [Int] [Int]

getCPUPercent :: CPUHandle -> Int -> IO (String, CPUHandle)
getCPUPercent (CPUH f a w) _ = do
  content <- readFile "/proc/stat"
  let d = map (map read) (map (drop 1) (map words (filter (\l -> isPrefixOf "cpu"l && ((>3) . length . head .words $ l)) (lines content)))) :: [[Int]]
  let all = map sum d
  let work = map sum (map (take 3) d)
  let cwork = zipWith (-) work w
  let call = zipWith (-) all a
  return (show (zipWith div (map (* 100) cwork) call), (CPUH f all work))

getCPUHandle :: IO CPUHandle
getCPUHandle = do
  file <- fopen "/proc/stat"
  return $CPUH file [0] [0]
