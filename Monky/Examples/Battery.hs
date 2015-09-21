{-|
Module      : Monky.Examples.Battery
Description : An example module instance for the battery module
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Battery ()
where

import Text.Printf (printf)

import Monky.Modules
import Monky.Battery

{- Battery Module -}
batteryColor :: BatteryState -> Int -> String
batteryColor BatLoading _ = "#009900"
batteryColor _ p
  | p < 20 = "#ffaf00"
  | p < 15 = "#ff8700"
  | p < 10 = "#ff5f00"
  | p <  5 = "#ff0000"
  | otherwise = ""

batterySymbol :: BatteryState -> Int -> String -> String
batterySymbol BatLoading _ user = "/home/" ++ user ++ "/.xmonad/xbm/ac_01.xbm"
batterySymbol _ p user
  | p < 50 = "/home/" ++ user ++ "/.xmonad/xbm/bat_low_01.xbm"
  | p < 20 = "/home/" ++ user ++ "/.xmonad/xbm/bat_empty_01.xbm"
  | otherwise = "/home/" ++ user ++ "/.xmonad/xbm/bat_full_01.xbm"

formatBatteryText :: String -> Int -> Int -> BatteryState -> Float -> String
formatBatteryText user p s online pow =
  printf "^fg(%s)^i(%s) %.1fW %3d%% %2d:%02d^fg()" (batteryColor online p) (batterySymbol online p user) pow p h m :: String
  where 
    h = s `div` 3600
    m = (s - h * 3600) `div` 60

getBatteryText :: String -> BatteryHandle -> IO String
getBatteryText user bh = do
  p <- getCurrentLevel bh
  s <- getTimeLeft bh
  online <- getCurrentStatus bh
  pow <- getLoading bh
  return (formatBatteryText user p s online pow)


-- |Example instance for battery module
instance Module BatteryHandle where
  getText = getBatteryText

