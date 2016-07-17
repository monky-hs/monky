{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.Alsa
Description : An example module instance for the alsa module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Alsa
  ( getVOLHandle
  )
where

import Data.IORef (atomicWriteIORef)

import Monky.Alsa hiding (getVOLHandle)
import qualified Monky.Alsa as A (getVOLHandle)
import Monky.Modules
import Monky.Examples.Utility

import Formatting
import Data.Text (Text)

{- ALSA module -}
getVolumeStr :: VOLHandle -> IO Text
getVolumeStr h = do
  updateVOLH h
  m <- getMute h
  v <- getVolumePercent h
  if m
    then return "Mute"
    else return $ sformat ((left 3 ' ' %. int) % "%") v

getVOLOutput :: VOLHandle -> IO [MonkyOut]
getVOLOutput h = do
  out <- getVolumeStr h
  return [MonkyPlain out]

instance PollModule AlsaH where
  getOutput (AH h) = getVOLOutput h

instance EvtModule AlsaH where
  startEvtLoop (AH h) r = do
    [fd] <- getPollFDs h
    s <- getOutput (AH h)
    atomicWriteIORef r s
    loopFd h fd r getVOLOutput

newtype AlsaH = AH VOLHandle

getVOLHandle :: String -- ^The audio-card to use
             -> IO AlsaH
getVOLHandle = fmap AH . A.getVOLHandle
