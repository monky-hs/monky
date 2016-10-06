{-
    Copyright 2015,2016 Markus Ongyerth

    This file is part of Monky.

    Monky is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Monky is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Monky.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.Sound.Alsa
Description : An example module instance for the alsa module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Sound.Alsa
  ( getVOLHandle
  , AlsaH
  )
where

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
    r =<< getOutput (AH h)
    loopFd h fd r getVOLOutput

-- |The handle type for this module
newtype AlsaH = AH VOLHandle

-- |Get a handle which allows access to audio (alsa) subsystem information
getVOLHandle :: String -- ^The audio-card to use
             -> IO AlsaH
getVOLHandle = fmap AH . A.getVOLHandle
