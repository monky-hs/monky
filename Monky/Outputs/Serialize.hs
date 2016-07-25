{-|
Module      : Monky.Outputs.Serialize
Description : Output module for storing
Maintainer  : ongy
Stability   : testing
Portability : Linux

This output can be used to transport monky-output over the network.

A simple receiver looks like:

@
{-# LANGUAGE OverloadedStrings #-}
import Monky.Modules
import Monky.Outputs.Dzen2

import qualified Data.ByteString.Lazy as BS
import Data.Serialize.Get
import Data.Serialize

printLine out bs = do
  let (Right (line, xs)) = runGetLazyState get bs
  doLine out line
  printLine out xs

main :: IO ()
main = do
  out <- getDzenOut 20 "\/home\/ongy\/.monky/xbm"
  input <- BS.getContents
  printLine out input
@

Used with:

@
socat - TCP4:127.0.0.1:1234 | .\/TestS| dzen2 -y 20 -w 1280
socat TCP-LISTEN:1234 "EXEC:monky -d \/tmp\/monky"
@
-}
module Monky.Outputs.Serialize
  ( getSerializeOut
  , SerializeOutput(..)
  )
where

import System.IO (hFlush, stdout)
import qualified Data.ByteString as BS
import Data.Serialize
import Monky.Modules

-- |The output handle for using show instance
data SerializeOutput = SerializeOutput

instance MonkyOutput SerializeOutput where
  doLine _ xs = do
    BS.putStr $ encode xs
    hFlush stdout

-- |Get an output handle for show formatting
getSerializeOut :: IO SerializeOutput
getSerializeOut = return SerializeOutput

