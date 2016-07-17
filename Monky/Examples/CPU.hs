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

  , getCPUHandleMany
  , getCPUHandleMany'
  , getNumaHandlesMany
  , getNumaHandlesMany'

  , getCPUHandleNoT
  , getCPUHandleNoT'
  , getNumaHandlesNoT
  , getNumaHandlesNoT'

  , C.ScalingType(..)
  )
where

import Data.Text (Text)
import Formatting
import Data.Composition ((.:))
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
  | p < 15 = "#009900"
  | p < 50 = "#ffff66"
  | p < 90 = "#ff6600"
  | otherwise = "#ff0000"

printBar :: Int -> MonkyOut
printBar h =
  MonkyColor ("#222222", cpuColor h) (MonkyBar h)

printXbm :: MonkyOut
printXbm = MonkyImage "cpu.xbm"

printFrequency :: Float -> MonkyOut
printFrequency = MonkyPlain . sformat (fixed 1 % "G")

printThemp :: Int -> MonkyOut
printThemp = MonkyPlain . sformat (" " % int % "°C") 


{- NORMAL -}
newtype CPUHandle  = CPH C.CPUHandle
newtype NumaHandle = NUH C.Numa

getCPUHandle :: C.ScalingType -> Maybe String -> IO CPUHandle
getCPUHandle = fmap CPH .: C.getCPUHandle

getCPUHandle' :: C.ScalingType -> IO CPUHandle
getCPUHandle' = fmap CPH . C.getCPUHandle'

-- |Get the Numa aware handle
getNumaHandles
  :: C.ScalingType
  -> [Maybe String] -- ^A list of thermal zones for our numa handles
  -> IO NumaHandle
getNumaHandles = fmap NUH .: C.getNumaHandles

getNumaHandles' :: C.ScalingType -> IO NumaHandle
getNumaHandles' = fmap NUH . C.getNumaHandles'

getNumaNode :: C.NumaHandle -> IO [MonkyOut]
getNumaNode nh = map printBar <$> C.getNumaPercent nh

getNumaText :: C.Numa -> IO [MonkyOut]
getNumaText (C.Numa xs) = do
  let ch = C.numaHandle . head $ xs
  nodes <- mapM getNumaNode xs
  ct <- C.getCPUTemp ch
  cf <- C.getCPUMaxScalingFreq ch
  return ([printXbm, printFrequency cf] ++ intercalate [(MonkyPlain " - ")] nodes  ++ [printThemp ct])

getCPUText :: C.CPUHandle -> IO [MonkyOut]
getCPUText ch = do
  cp <- C.getCPUPercent ch
  ct <- C.getCPUTemp ch
  cf <- C.getCPUMaxScalingFreq ch
  return ([printXbm, printFrequency cf] ++ map printBar cp ++ [printThemp ct])

instance PollModule CPUHandle where
  getOutput (CPH h) = getCPUText h

instance PollModule NumaHandle where
  getOutput (NUH h) = getNumaText h


{- MANY -}
newtype CPUMHandle  = CPHM C.CPUHandle
newtype NumaMHandle = NUHM C.Numa


getCPUHandleMany :: C.ScalingType -> Maybe String -> IO CPUMHandle
getCPUHandleMany = fmap CPHM .: C.getCPUHandle

getCPUHandleMany' :: C.ScalingType -> IO CPUMHandle
getCPUHandleMany' = fmap CPHM . C.getCPUHandle'

-- |Get the Numa aware handle
getNumaHandlesMany
  :: C.ScalingType
  -> [Maybe String] -- ^A list of thermal zones for our numa handles
  -> IO NumaMHandle
getNumaHandlesMany = fmap NUHM .: C.getNumaHandles

getNumaHandlesMany' :: C.ScalingType -> IO NumaMHandle
getNumaHandlesMany' = fmap NUHM . C.getNumaHandles'

getNumaNodeM :: C.NumaHandle -> IO [MonkyOut]
getNumaNodeM nh = do
  percs <- C.getNumaPercent nh
  return $ map printBar [maximum percs, sum percs `div` length percs]

getNumaTextM :: C.Numa -> IO [MonkyOut]
getNumaTextM (C.Numa xs) = do
  let ch = C.numaHandle . head $ xs
  nodes <- mapM getNumaNodeM xs
  ct <- C.getCPUTemp ch
  cf <- C.getCPUMaxScalingFreq ch
  return ([printXbm, printFrequency cf] ++ intercalate [(MonkyPlain " - ")] nodes ++ [printThemp ct])

getCPUTextM :: C.CPUHandle -> IO [MonkyOut]
getCPUTextM ch = do
  cp <- C.getCPUPercent ch
  let bs = [maximum cp, sum cp `div` length cp]
  ct <- C.getCPUTemp ch
  cf <- C.getCPUMaxScalingFreq ch
  return ([printXbm, printFrequency cf] ++ map printBar bs ++ [printThemp ct])

instance PollModule CPUMHandle where
  getOutput (CPHM h) = getCPUTextM h

instance PollModule NumaMHandle where
  getOutput (NUHM h) = getNumaTextM h


{- NOTemp -}
newtype CPUNHandle = CPHN C.CPUHandle
newtype NumaNHandle = NUHN C.Numa


getCPUHandleNoT :: C.ScalingType -> Maybe String -> IO CPUNHandle
getCPUHandleNoT = fmap CPHN .: C.getCPUHandle

getCPUHandleNoT' :: C.ScalingType -> IO CPUNHandle
getCPUHandleNoT' = fmap CPHN . C.getCPUHandle'

-- |Get the Numa aware handle
getNumaHandlesNoT
  :: C.ScalingType
  -> [Maybe String] -- ^A list of thermal zones for our numa handles
  -> IO NumaNHandle
getNumaHandlesNoT = fmap NUHN .: C.getNumaHandles

getNumaHandlesNoT' :: C.ScalingType -> IO NumaNHandle
getNumaHandlesNoT' = fmap NUHN . C.getNumaHandles'

getNumaTextN :: C.Numa -> IO [MonkyOut]
getNumaTextN (C.Numa xs) = do
  let ch = C.numaHandle . head $ xs
  nodes <- mapM getNumaNode xs
  cf <- C.getCPUMaxScalingFreq ch
  return ([printXbm, printFrequency cf] ++ intercalate [(MonkyPlain " - ")] nodes)

getCPUTextN :: C.CPUHandle -> IO [MonkyOut]
getCPUTextN ch = do
  cp <- C.getCPUPercent ch
  let bs = [maximum cp, sum cp `div` length cp]
  cf <- C.getCPUMaxScalingFreq ch
  return ([printXbm, printFrequency cf] ++ map printBar bs)

instance PollModule CPUNHandle where
  getOutput (CPHN h) = getCPUTextN h

instance PollModule NumaNHandle where
  getOutput (NUHN h) = getNumaTextN h

