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
  ( getConnH
  , Conn
  )
where

import Data.Composition ((.:))
import Data.Text (Text)
import Monky.Modules
import Monky.Connectivity hiding (getConnH)
import qualified Monky.Connectivity as C (getConnH)

showCon :: Bool -> Text
showCon False = "Unconnected"
showCon True  = "  Connected"

-- |The handle type for this module
newtype Conn = Conn ConnHandle

-- |Get a handle that allows testing for connectivity to a server
getConnH
  :: String -- ^The Host to use for connectivity probing
  -> Int -- ^Which port to use for connecivity probing (drop is bad)
  -> IO Conn
getConnH = fmap Conn .: C.getConnH

instance PollModule Conn where
  getOutput (Conn h) = fmap (\e -> [MonkyPlain $ showCon e]) $ hasConn h
