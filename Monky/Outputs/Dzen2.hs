{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Outputs/Dzen2
Description : Output module for dzen2
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides the output generation for piping into a dzen2 bar
-}
module Monky.Outputs.Dzen2
  ( DzenOutput
  , getDzenOut
  )
where

import Data.Composition ((.:))
import System.IO (hFlush, stdout)
import Monky.Modules

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- |The output handle for dzen2 pipe
data DzenOutput = DzenOutput Int Text

doOut :: DzenOutput -> MonkyOut -> IO ()
doOut _ (MonkyPlain t) = T.putStr t
doOut (DzenOutput _ p) (MonkyImage path _) = do
  T.putStr ("^i(" `T.append` p)
  T.putStr path
  T.putStr ".xbm) "
doOut (DzenOutput h _) (MonkyBar p) = do
  T.putStr "^p(3)^p(_TOP)^r(6x"
  putStr . show $ h - div (h * p) 100
  T.putStr ")^pa()"
doOut (DzenOutput h _) (MonkyHBar p) = do
  T.putStr "^r("
  putStr . show $ p
  T.putStr ("x" `T.append` (T.pack . show $ h `div` 2) `T.append` ")")
-- Reverse colours for HBar to support the way we draw them
doOut h (MonkyColor (f, b) (MonkyBar p)) = do
  T.putStr ("^bg(" `T.append` f `T.append` ")^fg(" `T.append` b `T.append` ")")
  doOut h (MonkyBar p)
  T.putStr "^bg()^fg()"
doOut h (MonkyColor (f, b) o) = do
  T.putStr ("^bg(" `T.append` b `T.append` ")^fg(" `T.append` f `T.append` ")")
  doOut h o
  T.putStr "^bg()^fg()"

doSegment :: DzenOutput -> [MonkyOut] -> IO ()
doSegment h = mapM_ (doOut h)

instance MonkyOutput DzenOutput where
  doLine _ [] = error "Why are you calling doLine without any modules? I don't think your config makes sense"
  doLine h [x] = do
    doSegment h x
    putStr "\n"
    hFlush stdout
  doLine h (x:xs) = do
    doSegment h x
    putStr " | "
    doLine h xs

-- |Get an output handle for dzen2 formatting
getDzenOut
  :: Int -- ^The height of your dzen bar in pixel (required for block-drawing)
  -> Text -- ^Path to the directory cointaining your .xbm files.
  -> IO DzenOutput
getDzenOut = return .: DzenOutput
