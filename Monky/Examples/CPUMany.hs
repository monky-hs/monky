{-|
Module      : Monky.Examples.CPU
Description : An example module instance for the cpu module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.CPUMany ()
where

import Text.Printf (printf)
import Data.List (intercalate)

import Monky.Modules
import Monky.CPU

-- TODO refactor all of this into a CPUCommon?
{- CPUMany (use average and max) Module -}
cpuColor :: Int -> String
cpuColor p
  | p < 15 = "#009900"
  | p < 50 = "#ffff66"
  | p < 90 = "#ff6600"
  | otherwise = "#ff0000"

barTemplate :: String
barTemplate = "^p(3)^pa(;0)^bg(%s)^r(6x8)^p(-6)^fg(#222222)^r(6x%d)^bg()^pa()^fg()"

printBar :: Int -> String
printBar pc = printf barTemplate (cpuColor pc) (16- div (16 * pc) 100)

printXbm :: String -> String
printXbm u = "^i(/home/" ++ u ++ "/.monky/xbm/cpu.xbm) "

printFrequency :: Float -> String
printFrequency = printf "%.1fG ^p(-3)"

printThemp :: Int -> String
printThemp = printf " %d°C"

formatCPUText :: [Int] -> Int -> Float -> String
formatCPUText cp ct cf = printFrequency cf ++ concatMap printBar cp ++ printThemp ct

getCPUText :: CPUHandle -> IO String
getCPUText ch = do
  cp <- getCPUPercent ch
  ct <- getCPUTemp ch
  cf <- getCPUMaxScalingFreq ch
  return (formatCPUText [maximum cp, sum cp `div` length cp] ct cf)

-- |Example instance for CPU module
instance Module CPUHandle where
  getText u = fmap (printXbm u ++) . getCPUText

getNumaText :: NumaHandle -> IO String
getNumaText ch = do
  cp <- getNumaPercent ch
  ct <- getCPUTemp (numaHandle ch)
  cf <- getCPUMaxScalingFreq (numaHandle ch)
  return (formatCPUText [maximum cp, sum cp `div` length cp] ct cf)

instance Module Numa where
  getText u (Numa handles) = do
    nodes <- mapM getNumaText handles
    return (printXbm u ++ (intercalate " - " nodes))
