{-|
Module      : Monky.Examples.Alsa
Description : An example module instance for the alsa module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Alsa ()
where

import Text.Printf (printf)

import Monky.Modules
import Monky.Alsa

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
