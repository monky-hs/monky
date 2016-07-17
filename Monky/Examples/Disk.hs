{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.Disk
Description : An example module instance for the disk module
Maintainer  : moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Disk
  ( getDiskHandle
  )
where

import Formatting

import Monky.Examples.Utility
import Monky.Modules
import Monky.Disk hiding (getDiskHandle)
import qualified Monky.Disk as D (getDiskHandle)

newtype DiskH = DH DiskHandle

getDiskHandle :: String -> IO DiskH
getDiskHandle = fmap DH . D.getDiskHandle

{- Disk module -}
instance PollModule DiskH where
  getOutput (DH dh) = do
    (dr, dw) <- getDiskReadWrite dh
    df <- getDiskFree dh
    return
      [ MonkyImage "diskette.xbm"
      , MonkyPlain $ sformat (stext % " " % stext % " " % stext) (convertUnitSI df "B") (convertUnitSI dr "B" ) (convertUnitSI dw "B")
      ]
