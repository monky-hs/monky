{-|
Module      : Monky.Examples.File
Description : An example module instance for the cpu module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.File
  ( FileHandle
  , getFileHandle
  )
where

import Monky.Modules
import Monky.Utility (fopen, readLine, File)

import qualified Data.Text.Encoding as T

newtype FileHandle = FH File

getFile :: FileHandle -> File
getFile (FH f) = f

getFileHandle :: String -> IO FileHandle
getFileHandle = fmap FH . fopen

instance PollModule FileHandle where
  getOutput h = do
    line <- readLine . getFile $ h
    return [MonkyPlain $ T.decodeUtf8 line]
