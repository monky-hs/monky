{-|
Module      : Monky.Examples.CPU
Description : An example module instance for the cpu module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.CPU ()
where

import Data.List (intercalate)
import Text.Printf (printf)

import Monky.Modules
import Monky.CPU

{- CPU Module -}
cpuColor :: Int -> String
cpuColor p
  | p < 15 = "#009900"
  | p < 50 = "#ffff66"
  | p < 90 = "#ff6600"
  | otherwise = "#ff0000"

barTemplate :: String
-- Dzen I hate your boxdrawing
barTemplate = "^p(3)^p(_TOP)^bg(%s)^fg(#222222)^r(6x%d)^bg()^fg()^pa()"


printBar :: Int -> String
-- this is only correct if we have 16 pixels on our dzen, but I will keep it for now
printBar pc = let height = (16 - div (16 * pc) 100) in
  printf barTemplate (cpuColor pc) height

printXbm :: String -> String
printXbm u = "^i(/home/" ++ u ++ "/.monky/xbm/cpu.xbm)"

printFrequency :: Float -> String
printFrequency = printf " %.1fG ^p(-3)"

printThemp :: Int -> String
printThemp = printf " %d°C"

formatCPUText :: [Int] -> Int -> Float -> String
formatCPUText cp ct cf = printFrequency cf ++ concatMap printBar cp ++ printThemp ct

getCPUText :: CPUHandle -> IO String
getCPUText ch = do
  cp <- getCPUPercent ch
  ct <- getCPUTemp ch
  cf <- getCPUMaxScalingFreq ch
  return (formatCPUText cp ct cf)

-- |Example instance for CPU module
instance Module CPUHandle where
  getText u h = fmap (printXbm u ++) $ getCPUText h

getNumaText :: NumaHandle -> IO String
getNumaText ch = do
  cp <- getNumaPercent ch
  ct <- getCPUTemp (numaHandle ch)
  cf <- getCPUMaxScalingFreq (numaHandle ch)
  return (formatCPUText cp ct cf)

instance Module Numa where
  getText u (Numa handles) = do
    nodes <- mapM getNumaText handles
    return (printXbm u ++ (intercalate " - " nodes))
