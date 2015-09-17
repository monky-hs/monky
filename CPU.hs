module CPU
(CPUHandle, getCPUHandle, getCPUPercent, getCPUTemp, getCPUMaxScalingFreq,
ScalingType(..))
where

import Utility (fopen, readValue, readContent, File)
import Data.List (isPrefixOf)
import Data.IORef
import Text.Printf (printf)
import Control.Monad (liftM2)

{- Stat temp freqencies work all-}
data CPUHandle = CPUH File File [File] (IORef [Int]) (IORef [Int])

data ScalingType = ScalingMax | ScalingCur

pathStat :: String
pathStat = "/proc/stat"

pathTemp :: String
pathTemp = "/sys/class/thermal/thermal_zone0/temp"

pathMaxScalingT :: String
pathMaxScalingT = "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_max_freq"

pathMaxScaling :: Int -> String
pathMaxScaling = printf pathMaxScalingT

pathCurScalingT :: String
pathCurScalingT = "/sys/devices/system/cpu/cpu%d/cpufreq/scaling_cur_freq"

pathCurScaling :: Int -> String
pathCurScaling = printf pathCurScalingT

getCPUFreqsCur :: Int -> IO [File]
getCPUFreqsCur 0 = return []
getCPUFreqsCur i = liftM2 (:) (fopen (pathCurScaling (i - 1))) (getCPUFreqsCur (i - 1))


getCPUFreqsMax :: Int -> IO [File]
getCPUFreqsMax 0 = return []
getCPUFreqsMax i = liftM2 (:) (fopen (pathMaxScaling (i - 1))) (getCPUFreqsMax (i - 1))


getCPUPercent :: CPUHandle -> IO [Int]
getCPUPercent (CPUH f _ _ aref wref) = do
  content <- readContent f
  let d = map ((map read . drop 1) . words) (filter (\l -> isPrefixOf "cpu"l && ((>3) . length . head . words $ l)) content) :: [[Int]]
  let sall = map sum d
  let work = map (sum . take 3) d
  a <- readIORef aref
  w <- readIORef wref
  let cwork = zipWith (-) work w
  let call = zipWith (-) sall a
  writeIORef wref work
  writeIORef aref sall
  return $zipWith div (map (* 100) cwork) call

getCPUTemp :: CPUHandle -> IO Int
getCPUTemp (CPUH _ f _ _ _) = do
  temp <- readValue f
  return $div temp 1000

getCPUMaxScalingFreq :: CPUHandle -> IO Float
getCPUMaxScalingFreq (CPUH _ _ files _ _) = do
  vals <- mapM readValue files
  return (fromIntegral (maximum vals) / 1000000)

getCPUFreqs :: ScalingType -> Int -> IO [File]
getCPUFreqs ScalingMax = getCPUFreqsMax
getCPUFreqs ScalingCur = getCPUFreqsCur


{- -1 at the end, because there is also the cpu line which sums over all -}
getNumberOfCores :: File -> IO Int
getNumberOfCores f = do
  stats <- readContent f
  return $length (filter (isPrefixOf "cpu") stats) - 1

getCPUHandle :: ScalingType -> IO CPUHandle
getCPUHandle t = do
  workref <- newIORef ([0] :: [Int])
  allref <- newIORef ([0] :: [Int])
  stat <- fopen pathStat
  temp <- fopen pathTemp
  num <- getNumberOfCores stat
  files <- getCPUFreqs t num
  return $CPUH stat temp files allref workref
