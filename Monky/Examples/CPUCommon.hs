{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.CPUCommon
Description : An example module instance for the cpu module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.CPUCommon
  ( formatCPUText
  , printXbm
  , formatNCPUText
  )
where

import Formatting

import Monky.Modules (MonkyOut(..))

import Data.Text (Text)
import qualified Data.Text as T

{- CPU Module -}
cpuColor :: Int -> Text
cpuColor p
  | p < 15 = "#009900"
  | p < 50 = "#ffff66"
  | p < 90 = "#ff6600"
  | otherwise = "#ff0000"

barTemplate :: Text -> Int -> [Text]
-- Dzen I hate your boxdrawing
barTemplate t h = 
  ["^p(3)^p(_TOP)^bg(", t, ")^fg(#222222)^r(6x", T.pack (show h), ")^bg()^fg()^pa()"]


printBar :: Int -> Text
-- this is only correct if we have 16 pixels on our dzen, but I will keep it for now
printBar pc = let height = (16 - div (16 * pc) 100) in
  T.concat $ barTemplate (cpuColor pc) height

printXbm :: String -> Text
printXbm u = "^i(/home/" `T.append` T.pack u `T.append` "/.monky/xbm/cpu.xbm) "

printFrequency :: Float -> Text
printFrequency = sformat (fixed 1 % "G")

printThemp :: Int -> Text
printThemp t = T.cons ' ' (T.pack $ show t) `T.append` "°C"

formatCPUText :: [Int] -> Int -> Float -> Text
formatCPUText cp ct cf = printFrequency cf `T.append` T.concat (map printBar cp) `T.append` printThemp ct

barNTemplate :: Int -> MonkyOut
-- Dzen I hate your boxdrawing
barNTemplate h =
  MonkyColor ("#222222", cpuColor h) (MonkyBar h)

printNXbm :: MonkyOut
printNXbm = MonkyImage "cpu.xbm"

printNFrequency :: Float -> MonkyOut
printNFrequency = MonkyPlain . sformat (fixed 1 % "G")

printNThemp :: Int -> MonkyOut
printNThemp = MonkyPlain . sformat (" " % int % "°C") 

formatNCPUText :: [Int] -> Int -> Float -> [MonkyOut]
formatNCPUText cp ct cf =
  printNXbm :printNFrequency cf :(map barNTemplate cp) ++ [printNThemp ct]

