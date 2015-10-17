{-|
Module      : Monky.Examples.CPUNoTemp
Description : An example module instance for the cpu module
Maintainer  : ongy
Stability   : testing
Portability : Linux

This version does not use the temperature option of the cpu handle
-}
module Monky.Examples.CPUNoTemp ()
where

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

formatCPUText :: String -> [Int] -> Float -> String
formatCPUText user cp cf =
  let bars = map printbars cp :: [String] in
  (freq ++ concat bars)
  where 
    printbars pc = printf "^p(3)^pa(;0)^bg(%s)^r(6x8)^p(-6)^fg(#222222)^r(6x%d)^bg()^pa()^fg()" (cpuColor pc) (16- div (16 * pc) 100) :: String
    freq = printf ("^i(/home/" ++ user ++ "/.monky/xbm/cpu.xbm) %.1fG ^p(-3)") cf :: String

getCPUText :: String -> CPUHandle -> IO String
getCPUText user ch = do
  cp <- getCPUPercent ch
  cf <- getCPUMaxScalingFreq ch
  return (formatCPUText user cp cf)

-- |Example instance for CPU module
instance Module CPUHandle where
  getText = getCPUText

