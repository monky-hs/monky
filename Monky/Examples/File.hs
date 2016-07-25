{-|
Module      : Monky.Examples.File
Description : Display the first line in a file each tick
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

-- |The handle type for this module
newtype FileHandle = FH File

getFile :: FileHandle -> File
getFile (FH f) = f

-- |Get the file handle
getFileHandle
  :: String -- ^Path to the file
  -> IO FileHandle
getFileHandle = fmap FH . fopen

instance PollModule FileHandle where
  getOutput h = do
    line <- readLine . getFile $ h
    return [MonkyPlain $ T.decodeUtf8 line]

-- TODO EvtModule with inotify?
