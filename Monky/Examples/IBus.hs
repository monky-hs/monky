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
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.Examples.IBus
Description : An example of how to use the ibus-hs package
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides an example of how to use the ibus-hs package.

This is only usefull if you have ibus installed and active on your
system.

This module will display the current global engine and provides
event driven updates.
For better usability the names returned by ibus can be remapped
to something more readable by the mapping given to the creation
function.
-}
module Monky.Examples.IBus
  ( getIBusH
  , IBusH
  )
where

import qualified Data.Text as T

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import DBus.Client (clientErrorMessage)
import Control.Exception (try)
import Control.Monad (void)

import Monky.Modules

import IBus
import IBus.EngineDesc

-- |The handle type for this module
data IBusH = IBusH IBusClient [(String, MonkyOut)]

instance PollModule IBusH where
  getOutput (IBusH h m) = do
    engine <- try $ engineName <$> getIBusEngine h
    case engine of
      (Left e) -> return [MonkyPlain . T.pack $ clientErrorMessage e]
      (Right x) -> return [remapEngine m x]

instance EvtModule IBusH where
  startEvtLoop ih@(IBusH h m) r = do
    r =<< getOutput ih
    void $ subscribeToEngine h $ \xs -> do
      let engine = head xs
      r [remapEngine m engine]

-- |Get an IBusH used by this module
getIBusH
  :: [(String, MonkyOut)] -- ^A mapping from engine names to display names for those engines
  -> IO IBusH -- ^The handle that can be given to 'pack'
getIBusH m = fmap (`IBusH` m) iBusConnect

remapEngine :: [(String, MonkyOut)] -> String -> MonkyOut
remapEngine [] x = MonkyPlain $ T.pack x
remapEngine ((l,r):xs) x = if l == x
  then r
  else remapEngine xs x
