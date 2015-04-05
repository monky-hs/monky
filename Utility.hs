module Utility (readValue, readValues, fopen, File, readLine, readLineStartingWith, readIntInLineStartingWith, readLineN, readContent)
where

import System.IO
import Text.Regex.Posix ((=~))
import Text.Regex (mkRegex, subRegex)

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
readLineN h n = do
  readLineN h (n-1)

readContent :: File -> IO String
readContent h = do
  hSeek h AbsoluteSeek 0
  content <- hGetContents h
  return content

readIntInLineStartingWith :: File -> String -> IO Int
readIntInLineStartingWith h s = do
  hSeek h AbsoluteSeek 0
  line <- readLineStartingWith h s
  let str = (subRegex (mkRegex "[^0-9]+") (subRegex (mkRegex ("[^0-9]+")) line "") "")
  return (read str :: Int)

fopen :: String -> IO File
fopen = flip openFile ReadMode

