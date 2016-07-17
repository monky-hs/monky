{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.Time
Description : An example module instance for the time module
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Time
  ( getTimeHandle
  , getFancyTimeHandle
  )
where

import Formatting
import qualified Data.Text as T

import Monky.Modules
import Monky.Time hiding (getTimeHandle)
import qualified Monky.Time as MT (getTimeHandle)

newtype THandle = TH TimeHandle

getTimeHandle :: String  -- ^The format that should be used for 'getTime' in strftime format
              -> IO THandle
getTimeHandle = fmap TH . MT.getTimeHandle

{- Time Module -}
instance PollModule THandle where
  getOutput (TH h) = do
    ts <- getTime h
    return
      [ MonkyImage "clock.xbm"
      , MonkyPlain $ T.pack ts
      ]

{- Time Module -}
timeToXBM :: (Int, Int) -> (Int, Int)
timeToXBM (h, m) = (xh, xm)
  where xh = h `mod` 12
        xm = m `div` 15

newtype FTHandle = FTH TimeHandle

getFancyTimeHandle :: String  -- ^The format that should be used for 'getTime' in strftime format
                   -> IO FTHandle
getFancyTimeHandle = fmap FTH . MT.getTimeHandle

instance PollModule FTHandle where
  getOutput (FTH h) = do
    ts <- getTime h
    t <- getHM h
    let (th, tm) = timeToXBM t
    return
      [ MonkyImage $ sformat (int % "-" % int % ".xbm") th tm
      , MonkyPlain . T.pack $ ts
      ]

