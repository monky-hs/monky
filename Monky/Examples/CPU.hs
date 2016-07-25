{-
    Copyright 2015,2016 Markus Ongyerth, Stephan Guenther

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
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.CPU
Description : An example module instance for the cpu module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.CPU
  ( getCPUHandle
  , getCPUHandle'
  , getNumaHandles
  , getNumaHandles'

  , C.ScalingType(..)

  , CPUHandle
  , NumaHandle

  , FreqHandle
  , TempHandle

  , getFreqHandle
  , getFreqNuma

  , getTempHandle
  , getTempHandle'
  , getTempHandles

  , getRawNumas
  , getRawCPU
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Formatting
import Data.List (intercalate)

import Monky.Modules
import qualified Monky.CPU as C

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

{- SHARED -}

{- CPU Module -}
cpuColor :: Int -> Text
cpuColor p
  | p < 15 = "#00d700"
  | p < 50 = "#ffff5f"
  | p < 90 = "#ffd700"
  | otherwise = "#ff0000"

printBar :: Int -> MonkyOut
printBar h =
  MonkyColor (cpuColor h, "#262626") (MonkyBar h)

printXbm :: MonkyOut
printXbm = MonkyImage "cpu" ' '--'🖩' Disabled because of compile errors

printFrequency :: Float -> MonkyOut
printFrequency = MonkyPlain . sformat (fixed 1 % "G")

printThemp :: Int -> MonkyOut
printThemp = MonkyPlain . sformat (" " % int % "°C")

getNumaNode :: C.NumaHandle -> IO [MonkyOut]
getNumaNode nh = map printBar <$> C.getNumaPercent nh

-- |Handle to get the cpu usage stats
newtype RawCPU = RawCPU C.CPUHandle
-- |Numa aware handle to get cpu usage stats
newtype RawNuma = RawNuma C.NumaHandle

instance PollModule RawCPU where
  getOutput (RawCPU h) =
    map printBar <$> C.getCPUPercent h

instance PollModule RawNuma where
  getOutput (RawNuma h) = getNumaNode h

-- |Handle to access cpu frequency information
newtype FreqHandle = FH C.FreqHandle
-- |Handle to acces thermal zone information
newtype TempHandle = TH C.TempHandle

instance PollModule FreqHandle where
  getOutput (FH fh) =
    return . printFrequency <$> C.getCPUMaxScalingFreq fh

instance PollModule TempHandle where
  getOutput (TH th) =
    return . printThemp <$> C.getCPUTemp th

{- NORMAL -}
-- |The handle type for the default setup
data CPUHandle  = CPH FreqHandle RawCPU TempHandle
-- |The handle type for the default setup (numa aware)
data NumaHandle = NUH [(FreqHandle, RawNuma, TempHandle)]

-- |Get a handle to access max node frequency
getFreqHandle :: C.ScalingType -> IO FreqHandle
getFreqHandle = fmap FH . C.getFreqHandle

-- |Numa away version of 'getFreqHandle'
getFreqNuma :: C.ScalingType -> RawNuma -> IO FreqHandle
getFreqNuma t (RawNuma h) = FH <$>  C.getFreqNuma t h

-- |Get the 'TempHandle' for the system (tries to guess CPU zone)
getTempHandle' :: IO TempHandle
getTempHandle' = getTempHandle . fromMaybe (error "Could not find thermal zone") =<< C.guessThermalZone

-- |Get a 'TempHandle' for each cpu termal zone on the system. will be infinite list, with invalidated zones for convinience
getTempHandles :: IO [TempHandle]
getTempHandles = fmap (map TH ) C.getThermalZones

-- |Get a 'TempHandle' for the specified thermal zone
getTempHandle :: String -> IO TempHandle
getTempHandle = fmap TH . C.getThermalZone

-- |Get a raw CPU handle
getRawCPU :: IO RawCPU
getRawCPU = RawCPU <$> C.getCPUHandle

-- |Get raw numa aware cpu handles
getRawNumas :: IO [RawNuma]
getRawNumas = map RawNuma <$> C.getNumaHandles

{- |Get a 'CPUHandle'

This is a shiny combination of the raw handles exported by this module
-}
getCPUHandle
  :: C.ScalingType -- ^The type of scaling frequency that should be reported
  -> String -- ^The thermal zone of the cpu
  -> IO CPUHandle
getCPUHandle s t = do
  fh <- getFreqHandle s
  raw <-  getRawCPU
  th <- getTempHandle t
  return $ CPH fh raw th

-- |Same as 'getCPUHandle' but tries to guess the thermal zone
getCPUHandle' :: C.ScalingType -> IO CPUHandle
getCPUHandle' s = do
  fh <- getFreqHandle s
  raw <-  getRawCPU
  th <- getTempHandle'
  return $ CPH fh raw th

-- |Numa aware version of 'getCPUHandle'
getNumaHandles
  :: C.ScalingType -- ^The type of scaling frequency that should be reported
  -> [String] -- ^A list of thermal zones for our numa handles
  -> IO NumaHandle
getNumaHandles st zones = do
  raw <- getRawNumas
  th <- mapM getTempHandle zones
  fh <- mapM (getFreqNuma st) raw
  return . NUH $ zip3 fh raw th

-- |Same as 'getNumaHandles' but tries to guess the thermal zone
getNumaHandles' :: C.ScalingType -> IO NumaHandle
getNumaHandles' st = do
  raw <- getRawNumas
  zones <- getTempHandles
  fh <- mapM (getFreqNuma st) raw
  return . NUH $ zip3 fh raw zones




formatNumaNode :: (FreqHandle, RawNuma, TempHandle) -> IO [MonkyOut]
formatNumaNode (fh, rh, th) = do
  freq <- getOutput fh
  raw <- getOutput rh
  temp <- getOutput th
  return (freq ++ raw ++ temp)

instance PollModule CPUHandle where
  getOutput (CPH fh rh th) = do
    cp <- getOutput rh
    ct <- getOutput th
    cf <- getOutput fh
    return (printXbm: cf ++ cp ++ ct)

instance PollModule NumaHandle where
  getOutput (NUH xs) =
    (printXbm:) . intercalate [(MonkyPlain (" - "))] <$> mapM formatNumaNode xs
