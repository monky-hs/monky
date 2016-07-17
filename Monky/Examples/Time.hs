{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.Time
Description : An example module instance for the time module
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Time ()
where

import qualified Data.Text as T

import Monky.Modules
import Monky.Time

{- Time Module -}
getTimeString :: String -> TimeHandle -> IO String
getTimeString user h = do
  ts <- getTime h
  return ("^i(/home/" ++ user ++ "/.monky/xbm/clock.xbm)  " ++ ts)

-- |Example instance for time module
instance Module TimeHandle where
    getText = getTimeString

instance NewModule TimeHandle where
  getOutput h = do
    ts <- getTime h
    return
      [ MonkyImage "clock.xbm"
      , MonkyPlain $ T.pack ts
      ]
