{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.Battery
Description : An example module instance for the battery module
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Battery
  ( getBatteryHandle
  , getBatteryHandle'
  , BatteryH
  )
where

import Formatting
import Data.Text (Text)
import Data.Composition ((.:))

import Monky.Modules
import Monky.Battery hiding (getBatteryHandle, getBatteryHandle')
import qualified Monky.Battery as B (getBatteryHandle, getBatteryHandle')

{- Battery Module -}
batteryColor :: BatteryState -> Int -> Text
batteryColor BatLoading _ = "#009900"
batteryColor _ p
  | p < 20 = "#ffaf00"
  | p < 15 = "#ff8700"
  | p < 10 = "#ff5f00"
  | p <  5 = "#ff0000"
  | otherwise = ""

batterySymbol :: BatteryState -> Int -> Text
batterySymbol BatLoading _ = "ac_01"
batterySymbol _ p
  | p < 50 = "bat_low_01"
  | p < 20 = "bat_empty_01"
  | otherwise = "bat_full_01"

instance PollModule BatteryH where
  getOutput (BH bh) = do
    p <- getCurrentLevel bh
    s <- getTimeLeft bh
    online <- getCurrentStatus bh
    pow <- getLoading bh
    let h = s `div` 3600
        m = (s `mod` 3600) `div` 60
    return 
      [ MonkyImage $ batterySymbol online p
      , MonkyColor (batteryColor online p, "") $
        MonkyPlain $ sformat (fixed 1 % "W " % int % "% " % (left 2 ' ' %. int) % ":" % (left 2 '0' %. int)) pow p h m
      ]

-- |The handle type for this module
newtype BatteryH = BH BatteryHandle

-- |Create a 'BatteryHandle'
getBatteryHandle :: String  -- ^The name of the wall socket adapter used by the battery
                 -> String -- ^The name of the battery
                 -> IO BatteryH
getBatteryHandle = fmap BH .: B.getBatteryHandle


-- |Version of 'getBatteryHandle' that defaults to "BAT0"
getBatteryHandle' :: String -> IO BatteryH
getBatteryHandle' = fmap BH . B.getBatteryHandle'
