{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.CPU
Description : An example module instance for the cpu module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.CPU ()
where

import Data.List (intersperse)

import Data.Text (Text)
import qualified Data.Text as T
import Monky.Examples.CPUCommon

import Monky.Modules
import Monky.CPU

getCPUText :: CPUHandle -> IO Text
getCPUText ch = do
  cp <- getCPUPercent ch
  ct <- getCPUTemp ch
  cf <- getCPUMaxScalingFreq ch
  return (formatCPUText cp ct cf)

getNumaText :: NumaHandle -> IO Text
getNumaText ch = do
  cp <- getNumaPercent ch
  ct <- getCPUTemp (numaHandle ch)
  cf <- getCPUMaxScalingFreq (numaHandle ch)
  return (formatCPUText cp ct cf)


-- |Example instance for CPU module
instance Module CPUHandle where
  getText u h = fmap (T.unpack . (printXbm u `T.append`)) $ getCPUText h

instance Module Numa where
  getText u (Numa handles) = do
    nodes <- mapM getNumaText handles
    return . T.unpack $ (printXbm u `T.append` (T.concat $ intersperse " - " nodes))


{- NEW STUFF #-}
getNCPUText :: CPUHandle -> IO [MonkyOut]
getNCPUText ch = do
  cp <- getCPUPercent ch
  ct <- getCPUTemp ch
  cf <- getCPUMaxScalingFreq ch
  return (formatNCPUText cp ct cf)


instance NewModule CPUHandle where
  getOutput = getNCPUText
