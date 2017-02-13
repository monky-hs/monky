{-
    Copyright 2015-2017 Markus Ongyerth, Stephan Guenther

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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.Utility
Description : Provides utility functions
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

This module provides utility functions used in monky modules
-}
module Monky.Utility
    ( readValue
    , readValues
    , fopen
    , fclose
    , File
    , readLine
    , readContent
    , findLine
    , splitAtEvery
    , maybeOpenFile
    , sdivBound
    , sdivUBound
    , listDirectory
    , C.UName
    , getUname
    , getKernelVersion
    )
where

import Data.Word (Word)
import qualified Monky.CUtil as C
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Data.List (isPrefixOf)

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

#if MIN_VERSION_base(4,9,0)
import System.Directory (listDirectory)
#else
import System.Directory (getDirectoryContents)

listDirectory :: String -> IO [String]
listDirectory = fmap (filter (not . ("." `isPrefixOf`))) . getDirectoryContents
#endif


class LineReadable a where
  hGetReadable :: Handle -> IO a

instance LineReadable String where
  hGetReadable = hGetLine

instance LineReadable ByteString where
  hGetReadable = BS.hGetLine

class FileReadable a where
  hGetFile :: Handle -> IO [a]

instance FileReadable String where
  hGetFile = readStringLines

instance FileReadable ByteString where
  hGetFile = readBSLines



-- |type alias to distinguish system functions from utility
newtype File = File Handle deriving (Show, Eq)

-- |Find a line in a list of Strings
findLine :: Eq a => [a] -> [[a]] -> Maybe [a]
findLine y (x:xs) = if y `isPrefixOf` x
  then Just x
  else findLine y xs
findLine _ [] = Nothing

-- |Read the first line of the file and convert it into an 'Int'
readValue :: File -> IO Int
readValue (File h) = do
  hSeek h AbsoluteSeek 0
  line <- hGetReadable h
  let value = fmap fst $ BS.readInt line
  return . fromMaybe (error ("Failed to read value from file:" ++ show h)) $ value

-- |Read the first line of the file and convert the words in it into 'Int's
readValues :: File -> IO [Int]
readValues (File h) = do
  hSeek h AbsoluteSeek 0
  line <- hGetReadable h
  let value = mapM (fmap fst . BS.readInt) $ BS.words line
  return . fromMaybe (error ("Failed to read values from file:" ++ show h)) $ value

-- |Read the first line of the file
readLine :: LineReadable a => File -> IO a
readLine (File h) = do
  hSeek h AbsoluteSeek 0
  hGetReadable h

-- |Read a File as String line by line
readStringLines :: Handle -> IO [String]
readStringLines h = do
  eof <- hIsEOF h
  if eof
    then return []
    else do
      l <- hGetReadable h
      fmap (l:) $ readStringLines h

-- |Read a File as ByteString line by line
readBSLines :: Handle -> IO [ByteString]
readBSLines h = fmap (BS.lines . BS.concat) $ readLines' []
  where
    readLines' ls = do
      ret <- BS.hGet h 512
      if ret == BS.empty
        then return $ reverse ls
        else readLines' (ret:ls)

-- |Rewind the file descriptor and read the complete file as lines
readContent :: FileReadable a => File -> IO [a]
readContent (File h) = do
  hSeek h AbsoluteSeek 0
  hGetFile h

-- |open a file read only
fopen :: String -> IO File
fopen = fmap File . flip openFile ReadMode

-- |Close a file opened by 'fopen'
fclose :: File -> IO ()
fclose (File h) = hClose h

-- |Split ys at every occurence of xs
splitAtEvery :: String -> String -> [String]
splitAtEvery s str = splitAtEvery' s str []
  where splitAtEvery' _ [] zs = [zs]
        splitAtEvery' xs (y:ys) zs = if xs `isPrefixOf` (y:ys)
          then zs:splitAtEvery' xs (cut ys) []
          else splitAtEvery' xs ys (zs ++ [y])
          where cut = drop (length xs -1)

-- |fmap fopen would give Maybe (IO File), this fixes that
maybeOpenFile :: Maybe String -> IO (Maybe File)
maybeOpenFile Nothing = return Nothing
maybeOpenFile (Just x) = fmap Just . fopen $ x

-- |0 save divide, uses maxbound for default value
sdivBound :: (Integral a, Bounded a) => a -> a -> a
sdivBound _ 0 = maxBound
sdivBound x y = x `div` y

-- |0 save divide, uses default value
sdivUBound :: Integral a => a -> a -> a -> a
sdivUBound _ 0 d = d
sdivUBound x y _ = x `div` y

infixl 7 `sdivBound`, `sdivUBound`

-- This should never change while we are on the same kernel
-- | Get the kernel name/version/release and so forth
getUname :: C.UName
getUname = unsafePerformIO C.uname

-- | Get the kernel version as (major, minor)
getKernelVersion :: (Word, Word)
getKernelVersion =
    let txt = T.splitOn (T.pack ".") . C._uRelease $ getUname
        (Right major: Right minor:_) = fmap T.decimal txt
     in (fst major, fst minor)
