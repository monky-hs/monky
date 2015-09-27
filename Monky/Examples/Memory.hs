{-|
Module      : Monky.Examples.Memory
Description : An example module instance for the memory module
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Memory ()
where

import Text.Printf (printf)

import Monky.Utility
import Monky.Modules
import Monky.Memory

{- Memory Module -}
getMemoryText :: String -> MemoryHandle -> IO String
getMemoryText user mh = do
  mp <- getMemoryAvailable mh
  return (printf ("^i(/home/" ++ user ++ "/.monky/xbm/mem.xbm) %s") (convertUnit (mp * 1000) "B" "K" "M" "G") :: String)


-- |Example instance for memory module
instance Module MemoryHandle where
  getText = getMemoryText


