{-|
Module      : Monky.Examples.Time
Description : An example module instance for the time module
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Time ()
where

import Text.Printf (printf)

import Monky.Modules
import Monky.Time

{- Time Module -}
timeToXBM :: (Int, Int) -> (Int, Int)
timeToXBM (h, m) = (xh, xm)
  where xh = h `mod` 12
        xm = m `div` 15

getTimeString :: String -> TimeHandle -> IO String
getTimeString user h = do
  ts <- getTime h
  t <- getHM h
  let (th, tm) = timeToXBM t
  return (printf ("^i(/home/" ++ user ++ "/.xmonad/xbm/%d-%d.xbm)  %s") th tm ts)

-- |Example instance for time module
instance Module TimeHandle where
    getText = getTimeString