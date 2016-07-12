{-
    Copyright 2015 Markus Ongyerth, Stephan Guenther

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
 , convertUnit
 , convertUnitB
 , convertUnitSI
 , findLine
 , splitAtEvery
 , maybeOpenFile
 , sdivBound
 , sdivUBound
 )
where

import System.IO
import Data.List (isPrefixOf)
import Text.Printf (printf)

import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

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
      bytes <- BS.hGet h 512
      if bytes == BS.empty
        then return $ reverse ls
        else readLines' (bytes:ls)


-- |Rewind the file descriptor and read the complete file as lines
readContent :: FileReadable a => File -> IO [a]
readContent (File h) = do
  hSeek h AbsoluteSeek 0
  hGetFile h


-- |Convert a number into a fixed length strings
convertUnit :: Int -> String -> String -> String -> String -> String
convertUnit = flip convertUnitI 1000 . fromIntegral


-- |Convert a number into a reasonable scale for binary units
convertUnitB :: Integral a => a -> String -> String
convertUnitB rate b = convertUnitI (fromIntegral rate) 1024 (' ':b) "ki" "Mi" "Gi"


-- |Convert a number into a reasonable scale for SI units
convertUnitSI :: Integral a => a -> String -> String
convertUnitSI rate b = convertUnitI (fromIntegral rate) 1000 b "k" "M" "G"


convertUnitI :: Float -> Int -> String -> String -> String -> String -> String
convertUnitI rate step bs ks ms gs
  | rate < fromIntegral (kf       ) = printf "%4.0f%s" rate bs
  | rate < fromIntegral (kf * 10  ) = printf "%4.2f%s" kv ks
  | rate < fromIntegral (kf * 100 ) = printf "%4.1f%s" kv ks
  | rate < fromIntegral (kf * 1000) = printf "%4.0f%s" kv ks
  | rate < fromIntegral (mf * 10  ) = printf "%4.2f%s" mv ms
  | rate < fromIntegral (mf * 100 ) = printf "%4.1f%s" mv ms
  | rate < fromIntegral (mf * 1000) = printf "%4.0f%s" mv ms
  | rate < fromIntegral (gf * 10  ) = printf "%4.2f%s" gv gs
  | rate < fromIntegral (gf * 100 ) = printf "%4.1f%s" gv gs
  | rate < fromIntegral (gf * 1000) = printf "%4.0f%s" gv gs
  | otherwise = printf "%4.0f%s" gv gs
  where
    kf = 1  * step
    mf = kf * step
    gf = mf * step
    kv = rate / fromIntegral kf
    mv = rate / fromIntegral mf
    gv = rate / fromIntegral gf


-- |open a file read only
fopen :: String -> IO File
fopen = fmap File . flip openFile ReadMode


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
