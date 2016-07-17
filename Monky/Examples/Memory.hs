{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.Memory
Description : An example module instance for the memory module
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Memory
  ( getMemoryHandle
  , getMemoryBarHandle
  )
where

import Monky.Examples.Utility
import Monky.Modules

import Monky.Memory hiding (getMemoryHandle)
import qualified Monky.Memory as M (getMemoryHandle)

newtype MHandle = MH MemoryHandle

getMemoryHandle :: IO MHandle
getMemoryHandle = fmap MH $ M.getMemoryHandle

{- Memory Module -}
instance PollModule MHandle where
  getOutput (MH h) = do
    mp <- getMemoryAvailable h
    return
      [ MonkyImage "mem.xbm"
      , MonkyPlain $ convertUnitB (mp * 1024) "B"
      ]

data MBHandle = MBH Float MemoryHandle

getMemoryBarHandle :: Float -> IO MBHandle
getMemoryBarHandle f = fmap (MBH f) $ M.getMemoryHandle


getUsagePercents :: MemoryHandle -> IO (Int,Int)
getUsagePercents h = do
  (total, avail, free, _) <- getMemoryStats h
  return ((total - avail) * 100 `div` total, (total - free) * 100 `div` total)

newBar :: Float -> (Int, Int) -> [MonkyOut]
newBar f (u, us) =
  let used   = round $ fromIntegral u / f
      cached = round $ fromIntegral (us - u) / f
      fill   = round (100 / f) - used - cached in
    [ MonkyHBar used
    , MonkyColor ("#444444", "") (MonkyHBar cached)
    , MonkyColor ("#222222", "") (MonkyHBar fill)
    ]

getNMemoryOut :: MemoryHandle -> Float -> IO [MonkyOut]
getNMemoryOut h f = do
  percents <- getUsagePercents h
  return $ MonkyImage "mem.xbm":newBar f percents

instance PollModule MBHandle where
  getOutput (MBH f h) = getNMemoryOut h f
