module CPU (CPUHandle, getCPUHandle, getCPUPercentTotal, getCPUPercent, getCPUDzenBars)
where

import Utility
import System.IO
import Text.Printf

data CPUHandle = CPUH File Int Int

getCPUPercentTotal :: CPUHandle -> IO (Int, CPUHandle)
getCPUPercentTotal (CPUH f a w) = do
  line <- readLine f
  let vals = (map read $ tail $ words line) :: [Int]
  let all = sum vals
  let work = sum $take 3 vals
  let cwork = work - w
  let call = all - a
  return (cwork*100 `div` call, CPUH f all work)

getCPUPercent :: CPUHandle -> Int -> IO (Int, CPUHandle)
getCPUPercent (CPUH f a w) n = do
  line <- readLineN f (n+1)
  --FIXME: seeking should be done by the helper
  hSeek f AbsoluteSeek 0
  let vals = (map read $ tail $ words line) :: [Int]
  let all = sum vals
  let work = sum $take 3 vals
  let cwork = work - w
  let call = all - a
  return (cwork*100 `div` call, CPUH f all work)

getCPUDzenBars :: CPUHandle -> Int -> IO (String, CPUHandle)
getCPUDzenBars (CPUH f a w) count = do
--  usage <- map (getCPUPercent (CPUH f a w)) [0..(count-1)]
  cpu0 <- getCPUPercent (CPUH f a w) 0 
  cpu1 <- getCPUPercent (CPUH f a w) 1
  let str = "^p(3)^pa(;0)^bg(#777777)^r(6x8)^p(-6)^fg(#222222)^r(6x" ++ show (16 - ((fst cpu0)*16) `div` 100) ++ ")^bg()^pa()^fg()"
  return (str, CPUH f a w)

getCPUHandle :: IO CPUHandle
getCPUHandle = do
  file <- fopen "/proc/stat"
  return $CPUH file 0 0
