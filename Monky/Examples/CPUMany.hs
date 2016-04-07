{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.CPU
Description : An example module instance for the cpu module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.CPUMany ()
where

import Data.List (intersperse)

import Data.Text (Text)
import qualified Data.Text as T
import Monky.Examples.CPUCommon
import Monky.Modules
import Monky.CPU

getNumaText :: NumaHandle -> IO Text
getNumaText ch = do
  cp <- getNumaPercent ch
  ct <- getCPUTemp (numaHandle ch)
  cf <- getCPUMaxScalingFreq (numaHandle ch)
  return (formatCPUText [maximum cp, sum cp `div` length cp] ct cf)


getCPUText :: CPUHandle -> IO Text
getCPUText ch = do
  cp <- getCPUPercent ch
  ct <- getCPUTemp ch
  cf <- getCPUMaxScalingFreq ch
  return (formatCPUText [maximum cp, sum cp `div` length cp] ct cf)

-- |Example instance for CPU module
instance Module CPUHandle where
  getText u = fmap (T.unpack . (printXbm u `T.append`)) . getCPUText

instance Module Numa where
  getText u (Numa handles) = do
    nodes <- mapM getNumaText handles
    return . T.unpack $ (printXbm u `T.append` (T.concat $ intersperse " - " nodes))
