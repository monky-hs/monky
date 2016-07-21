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
import Data.IORef

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
    atomicWriteIORef r =<< getOutput ih
    void $ subscribeToEngine h $ \xs -> do
      let engine = head xs
      atomicWriteIORef r [remapEngine m engine]

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
