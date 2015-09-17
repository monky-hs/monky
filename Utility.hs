module Utility (readValue, readValues, fopen, File, readLine, readLineStartingWith, readIntInLineStartingWith, readLineN, readContent, convertUnit)
where

import System.IO
import Text.Regex.Posix ((=~))
import Text.Regex (mkRegex, subRegex)
import Text.Printf (printf)

type File = Handle

readValue :: File -> IO Int
readValue h = do
  hSeek h AbsoluteSeek 0
  line <- hGetLine h
  return (read line :: Int)

readValues :: File -> IO [Int]
readValues h = do
  hSeek h AbsoluteSeek 0
  line <- hGetLine h
  return (map read $ words line)

readLine :: File -> IO String
readLine h = do
  hSeek h AbsoluteSeek 0
  hGetLine h

readLineStartingWith :: File -> String -> IO String
readLineStartingWith h s = do
  line <- hGetLine h
  if line =~ s then
    return line
  else
    readLineStartingWith h s

readLineN :: File -> Int -> IO String
readLineN h 0 = do
  line <- hGetLine h
  hSeek h AbsoluteSeek 0
  return line
readLineN h n = readLineN h (n-1)

readLineByLine :: File -> [String] -> IO [String]
readLineByLine h ls = do
  e <- hIsEOF h
  if e
  then return ls
  else do
    l <- hGetLine h
    readLineByLine h (ls ++ [l])

readContent :: File -> IO [String]
readContent h = do
  hSeek h AbsoluteSeek 0
  readLineByLine h []

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

readIntInLineStartingWith :: File -> String -> IO Int
readIntInLineStartingWith h s = do
  hSeek h AbsoluteSeek 0
  line <- readLineStartingWith h s
  let str = subRegex (mkRegex "[^0-9]+") (subRegex (mkRegex "[^0-9]+") line "") ""
  return (read str :: Int)

fopen :: String -> IO File
fopen = flip openFile ReadMode

