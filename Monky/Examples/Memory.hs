{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.Memory
Description : An example module instance for the memory module
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Memory ()
where

import qualified Data.Text as T

import Monky.Examples.Utility
import Monky.Modules
import Monky.Memory

{- Memory Module -}
getMemoryText :: String -> MemoryHandle -> IO String
getMemoryText user mh = do
  mp <- getMemoryAvailable mh
  return ("^i(/home/" ++ user ++ "/.monky/xbm/mem.xbm) " ++ (T.unpack $ convertUnitB (mp * 1024) "B"))


-- |Example instance for memory module
instance Module MemoryHandle where
  getText = getMemoryText

instance NewModule MemoryHandle where
  getOutput h = do
    mp <- getMemoryAvailable h
    return
      [ MonkyImage "mem.xbm"
      , MonkyPlain $ convertUnitB (mp * 1024) "B"
      ]
