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

  , getCPUHandleMany
  , getCPUHandleMany'
  , getNumaHandlesMany
  , getNumaHandlesMany'

  , getCPUHandleNoT
  , getNumaHandlesNoT

  , C.ScalingType(..)

  , CPUHandle
  , NumaHandle
  , CPUMHandle
  , NumaMHandle
  , CPUNHandle
  , NumaNHandle
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
-- |The handle type for the default setup
newtype CPUHandle  = CPH C.CPUHandle
-- |The handle type for the default setup (numa aware)
newtype NumaHandle = NUH C.Numa

-- |Get a 'CPUHandle'
getCPUHandle
  :: C.ScalingType -- ^The type of scaling frequency that should be reported
  -> Maybe String -- ^The thermal zone of the cpu
  -> IO CPUHandle
getCPUHandle = fmap CPH .: C.getCPUHandle

-- |Same as 'getCPUHandle' but tries to guess the thermal zone
getCPUHandle' :: C.ScalingType -> IO CPUHandle
getCPUHandle' = fmap CPH . C.getCPUHandle'

-- |Numa aware version of 'getCPUHandle'
getNumaHandles
  :: C.ScalingType -- ^The type of scaling frequency that should be reported
  -> [Maybe String] -- ^A list of thermal zones for our numa handles
  -> IO NumaHandle
getNumaHandles = fmap NUH .: C.getNumaHandles

-- |Same as 'getNumaHandles' but tries to guess the thermal zone
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
-- |A variant of 'CPUHandle' that merges CPU loads into (Average|MAX) usefule with high core count
newtype CPUMHandle  = CPHM C.CPUHandle
-- |Numa aware version of "CPUMHandle'
newtype NumaMHandle = NUHM C.Numa


-- |'getCPUHandle' for 'CPUMHandle'
getCPUHandleMany :: C.ScalingType -> Maybe String -> IO CPUMHandle
getCPUHandleMany = fmap CPHM .: C.getCPUHandle

-- |'getCPUHandle'' for 'CPUMHandle'
getCPUHandleMany' :: C.ScalingType -> IO CPUMHandle
getCPUHandleMany' = fmap CPHM . C.getCPUHandle'

-- |'getNumaHandles' for 'NumaMHandle'
getNumaHandlesMany
  :: C.ScalingType
  -> [Maybe String] -- ^A list of thermal zones for our numa handles
  -> IO NumaMHandle
getNumaHandlesMany = fmap NUHM .: C.getNumaHandles

-- |'getNumaHandles'' for 'NumaMHandle'
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
-- |A version of 'CPUHandle' that does not display the themperature, for instances when it's not exportet (VM)
newtype CPUNHandle = CPHN C.CPUHandle
-- |Numa aware version of 'CPUNHandle'
newtype NumaNHandle = NUHN C.Numa


-- |'getCPUHandle' for 'CPUNHandle'
getCPUHandleNoT :: C.ScalingType -> IO CPUNHandle
getCPUHandleNoT = fmap CPHN . flip C.getCPUHandle Nothing

-- |Get the Numa aware version of 'getCPUHandleNoT'
getNumaHandlesNoT
  :: C.ScalingType -> IO NumaNHandle
getNumaHandlesNoT = fmap NUHN . flip C.getNumaHandles [Nothing]

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

