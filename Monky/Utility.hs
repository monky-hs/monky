{-|
Module      : Utility
Description : Provides utility functions
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides utility functions used in monky modules
-}
module Monky.Utility
(readValue, readValues, fopen, File, readLine, readContent, convertUnit)
where

import System.IO
import Text.Printf (printf)

-- |type alias to distinguish system functions from utility
type File = Handle

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
convertUnit :: Int -> String -> String -> String ->String -> String
convertUnit rate u1 u2 u3 u4
  | rate < 1000 = printf "%4d%s" rate u1
  | rate < 10000 = printf "%4.2f%s" ((fromIntegral rate :: Float)/1000) u2
  | rate < 100000 = printf "%4.1f%s" ((fromIntegral rate :: Float)/1000) u2
  | rate < 1000000 = printf "%4d%s" (div rate 1000) u2
  | rate < 10000000 = printf "%4.2f%s" ((fromIntegral rate :: Float)/1000000) u3
  | rate < 100000000 = printf "%4.1f%s" ((fromIntegral rate :: Float)/1000000) u3
  | rate < 1000000000 = printf "%4d%s" (div rate 1000000) u3
  | rate < 10000000000 = printf "%4.2f%s" ((fromIntegral rate :: Float) / 1000000000) u4
  | rate < 100000000000 = printf "%4.1f%s" ((fromIntegral rate :: Float)/1000000000) u4
  | otherwise = printf "%4d%s" (div rate 1000000000) u4

-- |open a file read only
fopen :: String -> IO File
fopen = flip openFile ReadMode

