{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.MemoryBar
Description : An example module instance for the memory module displaying a bar
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.MemoryBar 
()
where

import Monky.Modules
import Monky.Memory

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Data.Text (Text)
import qualified Data.Text as T

-- Will return (used, used+)
-- where used+ = total - free <<- The %of memory currently used (caches included)
-- and   used  = total - avail <<- the %of memory bound
getUsagePercents :: MemoryHandle -> IO (Int,Int)
getUsagePercents h = do
  (total, avail, free, _) <- getMemoryStats h
  return ((total - avail) * 100 `div` total, (total - free) * 100 `div` total)


bar :: (Int, Int) -> Text
bar (u, us) =
  let totalB = printBar "" u
      availB = printBar avalC (us - u)
      freeB  = printBar freeC (100 - us) in
    totalB `T.append` availB `T.append` freeB `T.append` "^fg()"
  where freeC = "#222222"
        avalC = "#444444"


printBar :: Text -> Int -> Text
printBar c l =
  let colour = "^fg(" `T.append` c `T.append` ")^r("
      box = T.pack (show (l `div` 2)) `T.append` "x8)" in
    colour `T.append` box


{- Memory Module -}
getMemoryText :: String -> MemoryHandle -> IO Text
getMemoryText user mh = do
  percents <- getUsagePercents mh
  let xbm = ("^i(/home/" `T.append` T.pack user `T.append` "/.monky/xbm/mem.xbm) ")
  return (xbm `T.append` bar percents)


-- |Example instance for memory module
instance Module MemoryHandle where
  getText u h = T.unpack <$> getMemoryText u h


newBar :: (Int, Int) -> [MonkyOut]
newBar (u, us) =
  [ MonkyHBar (u `div` 2)
  , MonkyColor ("#444444", "") (MonkyHBar (us `div` 2))
  , MonkyColor ("#222222", "") (MonkyHBar ((100 - us) `div` 2))
  ]

getNMemoryOut :: MemoryHandle -> IO [MonkyOut]
getNMemoryOut h = do
  percents <- getUsagePercents h
  return $ MonkyImage "mem.xbm":newBar percents

instance NewModule MemoryHandle where
  getOutput = getNMemoryOut
