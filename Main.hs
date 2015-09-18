{-|
Module      : monky
Description : A conky clone
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

This application has a subset of the functionality conky proveds.

The goal of this application is to provide a standalone access to most system
values without spawning thousands of processes (looking at you conky).
The general design is to create handles at the start which encapsule state and
file descriptors which are used to get the system status.
-}
module Main
(main)
where


import Monky
import Modules
import Config
import Battery
import CPU
import Memory
import Network
import Time
import Alsa


-- |The list of modules
getModuleList :: [IO Modules]
getModuleList =
  [ pack $getVOLHandle "default"
  , pack $getCPUHandle ScalingCur
  , pack $getNetworkHandles network_devices
  , pack getMemoryHandle
  , pack getBatteryHandle
  , pack $getTimeHandle "%m/%d %k:%M:%S"
  ]


-- |Entrypoint for the application.
main :: IO()
main = startLoop getModuleList
