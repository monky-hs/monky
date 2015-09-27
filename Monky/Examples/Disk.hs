{-|
Module      : Monky.Examples.Disk
Description : An example module instance for the disk module
Maintainer  : moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Disk ()
where

import Text.Printf (printf)

import Monky.Utility
import Monky.Modules
import Monky.Disk

{- Disk module -}
formatDiskText :: String -> Int -> Int -> Int -> String
formatDiskText user dr dw df =
  printf "%s %s" eins zwei :: String
  where
    eins = printf ("^i(/home/" ++ user ++ "/.monky/xbm/diskette.xbm) %dG") df :: String
    zwei = printf "%s %s" (convertUnit dr  "B" "k" "M" "G") (convertUnit dw "B" "k" "M" "G") :: String

getDiskText :: String -> DiskHandle -> IO String
getDiskText u dh = do
  (dr, dw) <- getDiskReadWrite dh
  df <- getDiskFree dh
  return (formatDiskText u dr dw df)

-- |Example instance for disk module
instance Module DiskHandle where
  getText = getDiskText
