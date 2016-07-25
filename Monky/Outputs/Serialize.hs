{-
    Copyright 2016 Markus Ongyerth

    This file is part of Monky.

    Monky is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Monky is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Monky.  If not, see <http://www.gnu.org/licenses/>.
-}
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

