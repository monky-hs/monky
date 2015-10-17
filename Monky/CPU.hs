{-
    Copyright 2015 Markus Ongyerth, Stephan Guenther

    This file is part of Monky.

    Monky is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Monky is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Monky.  If not, see <http://www.gnu.org/licenses/>.
-}
{-|
Module      : Monky.CPU
Description : Allows access to information about the systems cpu
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.CPU
(CPUHandle, getCPUHandle, getCPUPercent, getCPUTemp, getCPUMaxScalingFreq,
ScalingType(..))
where

import Monky.Utility (fopen, readValue, readContent, File)
import Data.Char (isSpace)
import Data.List (isPrefixOf, findIndex)
import Data.Maybe (fromMaybe)
import Data.IORef
import Text.Printf (printf)
import Control.Monad (liftM2)

{- Stat temp freqencies work all-}
-- stat temp frequencies
-- |The handle exported by this module
data CPUHandle = CPUH File (Maybe File) [File] (IORef [Int]) (IORef [Int])

-- |Which values should be returned by getCPUFreq
data ScalingType 
  = ScalingMax -- ^Use the maximum frequencie allowed
  | ScalingCur -- ^Use the current frequencie used

pathStat :: String
pathStat = "/proc/stat"

pathTemp :: String -> String
pathTemp = printf "/sys/class/thermal/%s/temp"

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

-- |Get the cpu usage in percent for each (virtual) cpu
getCPUPercent :: CPUHandle -> IO [Int]
getCPUPercent (CPUH f _ _ aref wref) = do
  content <- readContent f
  let d = foldr fold [] content
  let sall = map sum d
  let work = map (sum . take 3) d
  a <- readIORef aref
  w <- readIORef wref
  let cwork = zipWith (-) work w
  let call = zipWith (-) sall a
  writeIORef wref work
  writeIORef aref sall
  return $zipWith (sdiv . (* 100)) cwork call
  where
    isCPU l = isPrefixOf "cpu" l && ((>3) . fromMaybe 0 $findIndex isSpace l)
    readVals = (map read . tail) . words
    fold x xs = if isCPU x then (readVals x :: [Int]):xs else xs
    sdiv x 0 = x
    sdiv x y = x `div` y

-- |get current CPU temperature
getCPUTemp :: CPUHandle -> IO Int
getCPUTemp (CPUH _ mf _ _ _) = case mf of
  Nothing -> error "Tried to get cpu temp while Nothing was given as zone"
  (Just f) -> do
    temp <- readValue f
    return (temp `div` 1000)


{- |this function returns a frequency according th the 'ScalingType' of the
handle.

The returned valued will be the max of all (virtual) proceessors on the system.
-}
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

maybeOpenFile :: Maybe String -> IO (Maybe File)
maybeOpenFile Nothing = return Nothing
maybeOpenFile (Just x) = fopen x >>= return . Just


getCPUHandle' :: ScalingType -> Maybe String -> IO CPUHandle
getCPUHandle' t xs = do
  workref <- newIORef ([0] :: [Int])
  allref <- newIORef ([0] :: [Int])
  stat <- fopen pathStat
  temp <- maybeOpenFile $pathTemp <$> xs
  num <- getNumberOfCores stat
  files <- getCPUFreqs t num
  return $CPUH stat temp files allref workref


-- |Create an 'CPUhandle'
getCPUHandle :: ScalingType -> IO CPUHandle
getCPUHandle = flip getCPUHandle' (Just "thermal_zone0")
