{-|
Module      : Monky.Outputs.Show
Description : Output module for storing
Maintainer  : ongy
Stability   : testing
Portability : Linux

Can be used to store output or transmit in a human readable form.

Simple receiver can be:

@
{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Monky.Modules
import Monky.Outputs.Dzen2

main :: IO ()
main = do
  out <- getDzenOut 20 "\/home\/ongy\/.monky\/xbm"
  input <- getContents
  mapM_ (doLine out . read) $ lines input
@

Used with:

> monky -d \/tmp\/monky | .\/Test | dzen2 -w 1280 -y 20

-}
module Monky.Outputs.Show
  ( getShowOut
  , ShowOutput(..)
  )
where

import System.IO (hFlush, stdout)
import Monky.Modules

-- |The output handle for using show instance
data ShowOutput = ShowOutput

instance MonkyOutput ShowOutput where
  doLine _ xs = do
    putStr $ show xs
    putStr "\n"
    hFlush stdout

-- |Get an output handle for show formatting
getShowOut :: IO ShowOutput
getShowOut = return ShowOutput
