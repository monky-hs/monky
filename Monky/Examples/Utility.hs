{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.Utility
Description : Provides utility functions for module implementations
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides utility functions used in monky modules
-}
module Monky.Examples.Utility
 ( loopFd
 , convertUnit
 , convertUnitB
 , convertUnitSI
 )
where

import Data.Text (Text)
import qualified Data.Text as T
import Formatting

import Control.Monad (unless)
import Control.Concurrent (threadWaitRead)
import System.Posix.Types (Fd)
import Monky.Modules


-- |Utility Function for eventing modules
loopFd
  :: h -- ^Some kind of module handle
  -> Fd -- ^The FD to block on
  -> ([MonkyOut] -> IO ()) -- ^The update consume function
  -> (h -> IO [MonkyOut]) -- ^The function to generate the output
  -> IO () -- ^This will loop for you
loopFd h fd r f = do
  threadWaitRead fd
  out <- f h
  unless (null out) (r out)
  loopFd h fd r f


-- |Convert a number into a fixed length strings
convertUnit :: Int -> Text -> Text -> Text -> Text -> Text
convertUnit = flip convertUnitT 1000 . fromIntegral


-- |Convert a number into a reasonable scale for binary units
convertUnitB :: Integral a => a -> Text -> Text
convertUnitB rate b = convertUnitT (fromIntegral rate) 1024 (" " `T.append` b) "ki" "Mi" "Gi"


-- |Convert a number into a reasonable scale for SI units
convertUnitSI :: Integral a => a -> Text -> Text
convertUnitSI rate b = convertUnitT (fromIntegral rate) 1000 b "k" "M" "G"

convertUnitT :: Float -> Int -> Text -> Text -> Text -> Text -> Text
convertUnitT rate step bs ks ms gs
  | rate < fromIntegral (kf       ) = sformat ((left 4 ' ' %. fixed 0) % stext) rate bs
  | rate < fromIntegral (kf * 10  ) = sformat ((left 4 ' ' %. fixed 2) % stext) kv ks
  | rate < fromIntegral (kf * 100 ) = sformat ((left 4 ' ' %. fixed 1) % stext) kv ks
  | rate < fromIntegral (kf * 1000) = sformat ((left 4 ' ' %. fixed 0) % stext) kv ks
  | rate < fromIntegral (mf * 10  ) = sformat ((left 4 ' ' %. fixed 2) % stext) mv ms
  | rate < fromIntegral (mf * 100 ) = sformat ((left 4 ' ' %. fixed 1) % stext) mv ms
  | rate < fromIntegral (mf * 1000) = sformat ((left 4 ' ' %. fixed 0) % stext) mv ms
  | rate < fromIntegral (gf * 10  ) = sformat ((left 4 ' ' %. fixed 2) % stext) gv gs
  | rate < fromIntegral (gf * 100 ) = sformat ((left 4 ' ' %. fixed 1) % stext) gv gs
  | rate < fromIntegral (gf * 1000) = sformat ((left 4 ' ' %. fixed 0) % stext) gv gs
  | otherwise = sformat ((left 4 ' ' %. expt 1) % stext) gv gs
  where
    kf = 1  * step
    mf = kf * step
    gf = mf * step
    kv = rate / fromIntegral kf
    mv = rate / fromIntegral mf
    gv = rate / fromIntegral gf


