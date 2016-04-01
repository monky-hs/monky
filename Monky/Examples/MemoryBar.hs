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
  (total, avail, free, _) <- getMemoryStats h
  return ((total - avail) * 100 `div` total, (total - free) * 100 `div` total)


bar :: (Int, Int) -> String
bar (u, us) = printBar "" u ++ printBar avalC (us - u) ++ printBar freeC (100 - us) ++ "^fg()"
  where freeC = "#222222"
        avalC = "#444444"


printBar :: String -> Int -> String
printBar c l = "^fg(" ++ c ++ ")^r(" ++ show (l `div` 2) ++ "x8)"


{- Memory Module -}
getMemoryText :: String -> MemoryHandle -> IO String
getMemoryText user mh =
  (("^i(/home/" ++ user ++ "/.monky/xbm/mem.xbm) ") ++) . bar <$> getUsagePercents mh


-- |Example instance for memory module
instance Module MemoryHandle where
  getText = getMemoryText

