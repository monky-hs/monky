{-# LANGUAGE CPP #-}
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

import Text.Printf (printf)

import Monky.Modules
import Monky.Memory
#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif


-- Will return (used, used+)
-- where used+ = total - free <<- The %of memory currently used (caches included)
-- and   used  = total - avail <<- the %of memory bound
getUsagePercents :: MemoryHandle -> IO (Int,Int)
getUsagePercents h = do
  total <- getMemoryTotal h
  avail <- getMemoryAvailable h
  free <- getMemoryFree h
  return ((total - avail) * 100 `div` total, (total - free) * 100 `div` total)

{- Memory Module -}
getMemoryText :: String -> MemoryHandle -> IO String
getMemoryText user mh =
  printf ("^i(/home/" ++ user ++ "/.monky/xbm/mem.xbm) %s") . bar <$> getUsagePercents mh
  where
    bar :: (Int, Int) -> String
    bar (u, us) = printBar "" u ++ printBar avalC (us - u) ++ printBar freeC (100 - us) ++ "^fg()"
    printBar :: String -> Int -> String
    printBar c l = printf "^fg(%s)^r(%dx8)" c (l `div` 2)
    freeC = "#222222"
    avalC = "#444444"

-- |Example instance for memory module
instance Module MemoryHandle where
  getText = getMemoryText

