{-|
Module      : Monky.Examples.Alsa
Description : An example module instance for the alsa module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Alsa ()
where

import Data.IORef (atomicWriteIORef)
import Text.Printf (printf)

import Monky.Alsa
import Monky.Modules
import Monky.Examples.Utility

import qualified Data.Text as T

{- ALSA module -}
getVolumeStr :: VOLHandle -> IO String
getVolumeStr h = do
  updateVOLH h
  m <- getMute h
  v <- getVolumePercent h
  if m
    then return "Mute"
    else return $printf "% 3d%%" v

-- |Example instance for alsa module
instance Module VOLHandle where
  getText _ = getVolumeStr
  getFDs = getPollFDs

instance NewModule VOLHandle where
  getOutput = getVOLOutput

getVOLOutput :: VOLHandle -> IO [MonkyOut]
getVOLOutput h = do
  out <- getVolumeStr h
  return [MonkyPlain $ T.pack out]

instance EvtModule VOLHandle where
  startEvtLoop h r = do
    [fd] <- getPollFDs h
    s <- getOutput h
    atomicWriteIORef r s
    loopFd h fd r getVOLOutput
