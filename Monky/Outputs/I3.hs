{-# LANGUAGE OverloadedStrings #-}
module Monky.Outputs.I3
where

import Monky.Modules

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (hFlush, stdout)

data I3Output = I3Output

barChar :: Int -> Char
barChar i
  | i <= 0                = ' '
  | i < (100 `div` 8)     = '▁'
  | i < (100 `div` 4)     = '▂'
  | i < (100 `div` 8 * 3) = '▃'
  | i < (100 `div` 2)     = '▄'
  | i < (100 `div` 8 * 5) = '▅'
  | i < (100 `div` 4 * 3) = '▆'
  | i < (100 `div` 8 * 7) = '▇'
  | otherwise             = '█'

hBarChar :: Int -> Char
hBarChar i
  | i < (100 `div` 8)     = '▏'
  | i < (100 `div` 4)     = '▎'
  | i < (100 `div` 8 * 3) = '▍'
  | i < (100 `div` 2)     = '▌'
  | i < (100 `div` 8 * 5) = '▋'
  | i < (100 `div` 4 * 3) = '▊'
  | i < (100 `div` 8 * 7) = '▉'
  | otherwise             = '█'

i3Full :: Text -> Text
i3Full xs = "\"full_text\": \"" `T.append` xs `T.append` "\""

getOut :: MonkyOut -> Text
getOut (MonkyPlain t)   = i3Full t
getOut (MonkyImage _ c)   = i3Full $ T.singleton c -- Images are not supported :(
getOut (MonkyBar p)     = i3Full $ T.singleton (barChar p)
getOut (MonkyHBar p)    = i3Full $ T.pack (replicate (p `div` 10) '█') `T.append` (T.singleton $ hBarChar (p `mod` 10 * 10))
getOut (MonkyColor (f, b) o) = T.concat
  [ getOut o
  , ", \"color\": \""
  , f
  , "\", \"background\": \""
  , b
  , "\""
  ]

doSegment :: [MonkyOut] -> IO ()
doSegment [] = return ()
doSegment [x] = do
  putChar '{'
  T.putStr $ getOut x
  putChar '}'
doSegment (x:xs) = do
  putChar '{'
  T.putStr $ getOut x
  putStr ", \"separator\": false"
  putStr ", \"separator_block_width\": 1"
  putStr "},"
  doSegment xs

outputLine :: [[MonkyOut]] -> IO ()
outputLine [] = error "i3-output outputLIne should never be called with an empty list"
outputLine [x] = doSegment x
outputLine (x:xs) = do
  doSegment x
  unless (null x) (putChar ',')
  outputLine xs

instance MonkyOutput I3Output where
  doLine _ xs = do
    putChar '['
    outputLine xs
    putStr "],"
    putChar '\n'
    hFlush stdout



getI3Output :: IO I3Output
getI3Output = do
  putStrLn "{\"version\":1}" -- Static version thingy we have to print
  putChar '[' -- Start the output lines array
  return I3Output
