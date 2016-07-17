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
  , Numa(..)
  , NumaHandle(..)
  , getCPUHandle
  , getCPUHandle'
  , getCPUPercent
  , getNumaPercent
  , getCPUTemp
  , getCPUMaxScalingFreq
  , ScalingType(..)
  , getNumaHandles
  , getNumaHandles'
  )
where

import Data.Char (isDigit)
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (getDirectoryContents)
import Monky.Utility
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe, fromJust)
import Data.IORef

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (readInt, words, unpack)

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif


{- Stat temp freqencies work all-}
-- stat temp frequencies
-- |The handle exported by this module
data CPUHandle = CPUH File (Maybe File) [File] (IORef [Int]) (IORef [Int])

-- Wrapper around numa nodes for stat reading
-- |Numa aware version of 'CPUHandle'
data NumaHandle = NumaHandle
  { numaCpus :: [String]
  , numaHandle :: CPUHandle
  }

{- Handles used for numa aware version -}
-- |Handle used for by numa aware version
newtype Numa = Numa [NumaHandle]

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


-- |Check if thermal zone is x86_pkg_temp
isX86PkgTemp :: String -> Bool
isX86PkgTemp xs = unsafePerformIO $do
  str <- readFile (thermalBaseP ++ xs ++ "/type")
  return (str == "x86_pkg_temp\n")

-- |Try to guess the thermal zones
guessThermalZones :: IO [String]
guessThermalZones = do
  filter isX86PkgTemp . filter ("thermal_zone" `isPrefixOf`) <$> tzones
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
readVals = map (fst . fromJust . BS.readInt) . tail


getPercent :: ([String] -> Bool) -> CPUHandle -> IO [Int]
getPercent f (CPUH file _ _ aref wref) = do
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
getCPUTemp :: CPUHandle -> IO Int
getCPUTemp (CPUH _ Nothing _ _ _) = return (-1)
getCPUTemp (CPUH _ (Just f) _ _ _) = do
  temp <- readValue f
  return (temp `div` 1000)

getMax :: [Int] -> Int
getMax = foldr max (-1)

{- |This function returns a frequency according the 'ScalingType' of the handle.

The returned valued will be the max of all (virtual) proceessors on the system.
-}
getCPUMaxScalingFreq :: CPUHandle -> IO Float
getCPUMaxScalingFreq (CPUH _ _ files _ _) = do
  vals <- mapM readValue files
  return (fromIntegral (getMax vals) / 1000000)


-- open the files to read the frequency from
-- TODO: breaking use Mabye ScalingType instead of ScalingNone
getCPUFreqs :: ScalingType -> [String] -> IO [File]
getCPUFreqs ScalingMax = getCPUFreqsMax
getCPUFreqs ScalingCur = getCPUFreqsCur
getCPUFreqs ScalingNone = (\_ -> return [])


getHandle :: String -> ScalingType -> Maybe String -> IO NumaHandle
getHandle path t xs = do
  cpus <- filter isCPU <$> getDirectoryContents path
  workref <- newIORef ([0] :: [Int])
  allref <- newIORef ([0] :: [Int])
  stat <- fopen pathStat
  temp <- maybeOpenFile $pathTemp <$> xs
  files <- getCPUFreqs t cpus
  return $ NumaHandle cpus (CPUH stat temp files allref workref)
  where isCPU ys = "cpu" `isPrefixOf` ys && all isDigit (drop 3 ys)

-- |Create an 'CPUHandle'
getCPUHandle
  :: ScalingType -- ^The scaling type, either "ScalingMax" or "ScalingCur"
  -> Maybe String -- ^The thermal zone to use or Nothing if there isn't any
  -> IO CPUHandle
getCPUHandle xs ys = numaHandle <$> getHandle pathCPUBase xs ys


-- |Version for getCPUHandle' that defaults to thermal zone "thermal_zone0"
getCPUHandle' :: ScalingType -> IO CPUHandle
getCPUHandle' s = getCPUHandle s =<< guessThermalZone

-- |Version for NumaAvare handles
getNumaHandle
  :: String -- ^The Node name
  -> ScalingType -- ^'ScalingType' as for getCPUHandle
  -> Maybe String -- ^thermal zone as for getCPUHandle
  -> IO NumaHandle -- ^A single numa node
getNumaHandle xs = getHandle (pathNumaBase ++ xs)

-- |Get the Numa aware handle
getNumaHandles
  :: ScalingType
  -> [Maybe String] -- ^A list of thermal zones for our numa handles
  -> IO Numa
getNumaHandles t xs = do
  nodes <- filter isNode <$> getDirectoryContents pathNumaBase
  handles <- sequence $ zipWith openNode nodes xs
  return $ Numa handles
  where isNode = ("node" `isPrefixOf`)
        openNode node thermal = getNumaHandle node t thermal

-- |getnumaHandles' but try to guess the thermal zones
getNumaHandles' :: ScalingType -> IO Numa
getNumaHandles' t = do
  zones <- guessThermalZones
  let tzones = map Just zones ++ repeat Nothing
  getNumaHandles t tzones
