{-# LANGUAGE OverloadedStrings #-}
module Monky.Outputs.Dzen2
  ( DzenOutput
  , getDzenOut
  )
where

import System.IO (hFlush, stdout)
import Monky.Modules

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- |The output handle, the int is a height hint since dzen has no percent movements
data DzenOutput = DzenOutput Int Text

doOut :: DzenOutput -> MonkyOut -> IO ()
doOut _ (MonkyPlain t) = T.putStr t
doOut (DzenOutput _ p) (MonkyImage path) = do
  T.putStr ("^i(" `T.append` p)
  T.putStr path
  T.putStr ") "
doOut (DzenOutput h _) (MonkyBar p) = do
  T.putStr "^p(3)^p(_TOP)^r(6x"
  putStr . show $ h - div (h * p) 100
  T.putStr ")^pa()"
doOut (DzenOutput h _) (MonkyHBar p) = do
  T.putStr "^r("
  putStr . show $ p
  T.putStr ("x" `T.append` (T.pack . show $ h `div` 2) `T.append` ")")
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

getDzenOut :: Int -> Text -> DzenOutput
getDzenOut = DzenOutput
