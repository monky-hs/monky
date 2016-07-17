{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : Monky.Examples.Connectivity
Description : Simple Connectivity example
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Connectivity
  (
  )
where

import Monky.Modules
import Monky.Connectivity

import Data.String

showCon :: forall t. Data.String.IsString t => Bool -> t
showCon False = "Unconnected"
showCon True  = "  Connected"

instance Module ConnHandle where
  getText _ = fmap showCon . hasConn

instance NewModule ConnHandle where
  getOutput = fmap (\e -> [MonkyPlain $ showCon e]) . hasConn
