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
getTimeString :: String -> TimeHandle -> IO String
getTimeString user h = do
  ts <- getTime h
  return (printf ("^i(/home/" ++ user ++ "/.monky/xbm/clock.xbm)  %s") ts)

-- |Example instance for time module
instance Module TimeHandle where
    getText = getTimeString
