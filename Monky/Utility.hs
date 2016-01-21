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
 , File
 , readLine
 , readContent
 , convertUnit
 , convertUnitB
 , convertUnitSI
 , findLine
 , splitAtEvery
 )
where

import System.IO
import Data.List (isPrefixOf)
import Text.Printf (printf)

-- |type alias to distinguish system functions from utility
type File = Handle

-- |Find a line in a list of Strings

findLine :: String -> [String] -> Maybe String
findLine y (x:xs) = if y `isPrefixOf` x then Just x else findLine y xs
findLine _ [] = Nothing

-- |Read the first line of the file and convert it into an 'Int'
readValue :: File -> IO Int
readValue h = do
  hSeek h AbsoluteSeek 0
  line <- hGetLine h
  return (read line :: Int)

-- |Read the first line of the file and convert the words in it into 'Int's
readValues :: File -> IO [Int]
readValues h = do
  hSeek h AbsoluteSeek 0
  line <- hGetLine h
  return (map read $ words line)

-- |Read the first line of the file
readLine :: File -> IO String
readLine h = do
  hSeek h AbsoluteSeek 0
  hGetLine h

-- |Internal function for readContent
readLineByLine :: File -> [String] -> IO [String]
readLineByLine h ls = do
  eof <- hIsEOF h
  if eof
    then return ls
    else do
      l <- hGetLine h
      readLineByLine h (ls ++ [l])


-- |Rewind the file descriptor and read the complete file as lines
readContent :: File -> IO [String]
readContent h = do
  hSeek h AbsoluteSeek 0
  readLineByLine h []


-- |Convert a number into a reasonable scale for SI units
convertUnit :: Int -> String -> String -> String -> String -> String
convertUnit = flip convertUnitI 1000 . fromIntegral


-- |Convert a number into a reasonable scale for binary units
convertUnitB :: Int -> String -> String
convertUnitB rate b = convertUnitI (fromIntegral rate) 1024 (' ':b) "ki" "Mi" "Gi"


convertUnitSI :: Int -> String -> String
convertUnitSI rate b = convertUnitI (fromIntegral rate) 1000 b "k" "M" "G"


convertUnitI :: Float -> Float -> String -> String -> String -> String -> String
convertUnitI rate step bs ks ms gs
  | rate < kf = printf "%4.0f%s" rate bs
  | rate < kf * 10 = printf "%4.2f%s" kv ks
  | rate < kf * 100 = printf "%4.1f%s" kv ks
  | rate < kf * 1000 = printf "%4.0f%s" kv ks
  | rate < mf * 10 = printf "%4.2f%s" mv ms
  | rate < mf * 100 = printf "%4.1f%s" mv ms
  | rate < mf * 1000 = printf "%4.0f%s" mv ms
  | rate < gf * 10 = printf "%4.2f%s" gv gs
  | rate < gf * 100 = printf "%4.1f%s" gv gs
  | rate < gf * 1000 = printf "%4.0f%s" gv gs
  | otherwise = printf "%4.0f%s" gv gs
  where
    kf = step
    mf = step * step
    gf = step * step * step
    kv = rate / kf
    mv = rate / mf
    gv = rate / gf


-- |open a file read only
fopen :: String -> IO File
fopen = flip openFile ReadMode


-- |Split ys at every occurence of xs
splitAtEvery :: String -> String -> [String]
splitAtEvery s str = splitAtEvery' s str []
  where splitAtEvery' _ [] zs = [zs]
        splitAtEvery' xs (y:ys) zs = if xs `isPrefixOf` (y:ys)
          then zs:splitAtEvery' xs (cut ys) []
          else splitAtEvery' xs ys (zs ++ [y])
          where cut = drop (length xs -1)
