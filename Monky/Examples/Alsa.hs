{-# LANGUAGE OverloadedStrings #-}
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

import Monky.Alsa
import Monky.Modules
import Monky.Examples.Utility

import Formatting
import Data.Text (Text)
import qualified Data.Text as T

{- ALSA module -}
getVolumeStr :: VOLHandle -> IO Text
getVolumeStr h = do
  updateVOLH h
  m <- getMute h
  v <- getVolumePercent h
  if m
    then return "Mute"
    else return $ sformat ((left 3 ' ' %. int) % "%") v

-- |Example instance for alsa module
instance Module VOLHandle where
  getText _ = fmap T.unpack . getVolumeStr
  getFDs = getPollFDs

instance NewModule VOLHandle where
  getOutput = getVOLOutput

getVOLOutput :: VOLHandle -> IO [MonkyOut]
getVOLOutput h = do
  out <- getVolumeStr h
  return [MonkyPlain out]

instance EvtModule VOLHandle where
  startEvtLoop h r = do
    [fd] <- getPollFDs h
    s <- getOutput h
    atomicWriteIORef r s
    loopFd h fd r getVOLOutput
