{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.Disk
Description : An example module instance for the disk module
Maintainer  : moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Disk ()
where

import Formatting
import qualified Data.Text as T

import Monky.Examples.Utility
import Monky.Modules
import Monky.Disk

{- Disk module -}
formatDiskText :: String -> Int -> Int -> Int -> String
formatDiskText user dr dw df = eins ++ ' ' : zwei
  where
    eins :: String
    eins = "^i(/home/" ++ user ++ "/.monky/xbm/diskette.xbm) " ++ (T.unpack $ convertUnit df "B" "k" "M" "G")
    zwei :: String
    zwei = (T.unpack $ convertUnit dr  "B" "k" "M" "G") ++ ' ' : (T.unpack $ convertUnit dw "B" "k" "M" "G")

getDiskText :: String -> DiskHandle -> IO String
getDiskText u dh = do
  (dr, dw) <- getDiskReadWrite dh
  df <- getDiskFree dh
  return (formatDiskText u dr dw df)

-- |Example instance for disk module
instance Module DiskHandle where
  getText = getDiskText

instance NewModule DiskHandle where
  getOutput dh = do
    (dr, dw) <- getDiskReadWrite dh
    df <- getDiskFree dh
    return
      [ MonkyImage "diskette.xbm"
      , MonkyPlain $ sformat (stext % " " % stext % " " % stext) (convertUnitSI df "B") (convertUnitSI dr "B" ) (convertUnitSI dw "B")
      ]
