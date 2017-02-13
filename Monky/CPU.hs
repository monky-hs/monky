{-
    Copyright 2015-2017 Markus Ongyerth, Stephan Guenther

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
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.CPU
Description : Allows access to information about the systems cpu
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.CPU
  ( CPUHandle
  , TempHandle
  , FreqHandle
  , NumaHandle(..)
  , getCPUHandle
  , getCPUPercent
  , getNumaPercent
  , getCPUTemp
  , getCPUMaxScalingFreq
  , ScalingType(..)
  , getNumaHandles

  , guessThermalZone
  , getThermalZone
  , getThermalZones

  , getFreqHandle
  , getFreqNuma
  )
where

import Data.Char (isDigit)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (getDirectoryContents)
import Monky.Utility
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.IORef

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (readInt, words, unpack)

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif


{- Stat work all-}
-- |The handle exported by this module
data CPUHandle = CPUH File (IORef [Int]) (IORef [Int])
-- |The handle for themperature
newtype TempHandle = TH (Maybe File)
-- |The handle for cpu frequency
newtype FreqHandle = FH [File]

-- Wrapper around numa nodes for stat reading
-- |Numa aware version of 'CPUHandle'
data NumaHandle = NumaHandle
  { numaCpus :: [String]
  , numaHandle :: CPUHandle
  }


-- |Which values should be returned by getCPUFreq
data ScalingType
  = ScalingMax -- ^Use the maximum frequencie allowed
  | ScalingCur -- ^Use the current frequencie used
  | ScalingNone -- ^Don't open any files for scaling type


pathStat :: String
pathStat = "/proc/stat"

pathCPUBase :: String
pathCPUBase = "/sys/devices/system/cpu"

thermalBaseP :: String
thermalBaseP = "/sys/class/thermal/"

pathTemp :: String -> String
pathTemp zone = thermalBaseP ++ zone ++ "/temp"

pathMaxScaling :: String -> String
pathMaxScaling str = "/sys/devices/system/cpu/" ++ str ++ "/cpufreq/scaling_max_freq"

pathCurScaling :: String -> String
pathCurScaling str = "/sys/devices/system/cpu/" ++ str ++ "/cpufreq/scaling_cur_freq"

pathNumaBase :: String
pathNumaBase = "/sys/devices/system/node/"

getCPUFreqsCur :: [String] -> IO [File]
getCPUFreqsCur = mapM (fopen . pathCurScaling)

getCPUFreqsMax :: [String] -> IO [File]
getCPUFreqsMax = mapM (fopen . pathMaxScaling)


-- | Check if thermal zone is x86_pkg_temp
checkType :: (String -> Bool) ->  String -> Bool
checkType f xs = unsafePerformIO $ do
    f <$> readFile (thermalBaseP ++ xs ++ "/type")

-- | Get the filter function for the thermal zone getter
thermalGuesser :: String -> Bool
thermalGuesser =
    let (ma, mi) = getKernelVersion
     in if ma <= 3 && mi <= 13
           then checkType ("pkg-temp-" `isPrefixOf`)
           else checkType (== "x86_pkg_temp\n")

-- | Try to guess the thermal zones
guessThermalZones :: IO [String]
guessThermalZones = do
  filter thermalGuesser . filter ("thermal_zone" `isPrefixOf`) <$> tzones
  where tzones = getDirectoryContents thermalBaseP

-- |Tries to guess the thermal zone based on type
guessThermalZone :: IO (Maybe String)
guessThermalZone = fmap listToMaybe guessThermalZones

-- |Calcualate the work done by the cores (all, work)
calculateWork :: [[Int]] -> ([Int], [Int])
calculateWork xs =
  let work = map (sum . take 3) xs
      sall  = zipWith (\x y -> x + sum y) work (map (drop 3) xs) in
    (sall, work)


calculatePercent :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
calculatePercent sall work owork oall =
  let cwork = zipWith (-) work owork
      call  = zipWith (-) sall oall in
    zipWith (sdivBound . (* 100)) cwork call


readVals :: [ByteString] -> [Int]
readVals = map (fst . fromMaybe (error "CPUModule: Something in /proc/stat was unexpted") . BS.readInt) . tail


getPercent :: ([String] -> Bool) -> CPUHandle -> IO [Int]
getPercent f (CPUH file aref wref) = do
  content <- map BS.words <$> readContent file
  let cpus = filter (f . map BS.unpack) content
  let d = map readVals cpus
  let (sall, work) = calculateWork d
  a <- readIORef aref
  w <- readIORef wref
  writeIORef wref work
  writeIORef aref sall
  return $ calculatePercent sall work w a

-- |Get the cpu usage in percent for each (virtual) cpu
getCPUPercent :: CPUHandle -> IO [Int]
getCPUPercent = getPercent (\(x:_) -> "cpu" `isPrefixOf` x && length x > 3)

-- |Read node information
getNumaPercent :: NumaHandle -> IO [Int]
getNumaPercent (NumaHandle cpus h) =
  getPercent (\xs -> head xs `elem` cpus) h

-- |get current CPU temperature
getCPUTemp :: TempHandle -> IO Int
getCPUTemp (TH Nothing) = return (-1)
getCPUTemp (TH (Just f)) = do
  temp <- readValue f
  return (temp `div` 1000)


getMax :: [Int] -> Int
getMax = foldr max (-1)

{- |This function returns a frequency according the 'ScalingType' of the handle.

The returned valued will be the max of all (virtual) proceessors on the system.
-}
getCPUMaxScalingFreq :: FreqHandle -> IO Float
getCPUMaxScalingFreq (FH files) = do
  vals <- mapM readValue files
  return (fromIntegral (getMax vals) / 1000000)

-- open the files to read the frequency from
getCPUFreqs :: ScalingType -> [String] -> IO [File]
getCPUFreqs ScalingMax = getCPUFreqsMax
getCPUFreqs ScalingCur = getCPUFreqsCur
getCPUFreqs ScalingNone = (\_ -> return [])


getCPUs :: String -> IO [String]
getCPUs = fmap (filter isCPU) . getDirectoryContents
  where isCPU ys = "cpu" `isPrefixOf` ys && all isDigit (drop 3 ys)


-- |Get a frequency handle by type
getFreqHandle :: ScalingType -> IO FreqHandle
getFreqHandle t = do
  cpus <- getCPUs pathCPUBase
  FH <$> getCPUFreqs t cpus

-- |Get a frequency handle limited to the cpus the numa handle uses
getFreqNuma :: ScalingType -> NumaHandle -> IO FreqHandle
getFreqNuma t (NumaHandle cpus _) =
  FH <$> getCPUFreqs t cpus


getHandle :: String -> IO NumaHandle
getHandle path = do
  getNumaHandle =<< getCPUs path

-- |Raw-ish access to numa handle, this can be (ab)used to make your own filter on cpus
getNumaHandle
  :: [String] -- ^CPU "names" to include ([cpu0, cpu1, cpu2, ..])
  -> IO NumaHandle
getNumaHandle cpus = do
  workref <- newIORef ([0] :: [Int])
  allref <- newIORef ([0] :: [Int])
  stat <- fopen pathStat
  return $ NumaHandle cpus (CPUH stat allref workref)

-- |Create an 'CPUHandle'
getCPUHandle :: IO CPUHandle
getCPUHandle = numaHandle <$> getHandle pathCPUBase

-- |Get the CPUs thermal zone
getThermalZone :: String -> IO TempHandle
getThermalZone = fmap (TH . Just) . fopen . pathTemp

-- |Get the CPUs thermal zones, will be same order as numa nodes (hopefully)
getThermalZones :: IO [TempHandle]
getThermalZones = do
  real <- mapM (fmap (TH . Just) . fopen . pathTemp) =<< guessThermalZones
  return (real ++ repeat (TH Nothing))

-- |Get the Numa aware handle
getNumaHandles :: IO [NumaHandle]
getNumaHandles = do
  nodes <- filter isNode <$> getDirectoryContents pathNumaBase
  mapM (getHandle . (pathNumaBase ++)) nodes
  where isNode = ("node" `isPrefixOf`)
