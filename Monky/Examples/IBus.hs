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
import System.Posix.IO
import System.Posix.Types (Fd)

import IBus
import IBus.EngineDesc


data IBusH = IBusH IBusClient [(String, String)]

instance NewModule IBusH where
  getOutput (IBusH h m) = do
    engine <- try $ engineName <$> getIBusEngine h
    case engine of
      (Left e) -> return [MonkyPlain . T.pack $ clientErrorMessage e]
      (Right x) -> return [MonkyImage . T.pack $ remapEngine m x]

instance EvtModule IBusH where
  startEvtLoop ih@(IBusH h m) r = do
    atomicWriteIORef r =<< getOutput ih
    void $ subscribeToEngine h $ \xs -> do
      let engine = head xs
      atomicWriteIORef r [MonkyImage . T.pack $ remapEngine m engine]

instance Module IBusH where
  getText = getText'
  getFDs = getFD
  getEventText = getEventText'

-- |Get an IBusH used by this module
getIBusH
  :: [(String, String)] -- ^A mapping from engine names to display names for those engines
  -> IO IBusH -- ^The handle that can be given to 'pack'
getIBusH m = fmap (`IBusH` m) iBusConnect

getFD :: IBusH -> IO [Fd]
getFD (IBusH h _) = do
  (r, w) <- createPipe
  _ <- subscribeToEngine
    h
    (\xs -> void $ fdWrite w (head xs ++ "\n"))
  return [r]

remapEngine :: [(String, String)] -> String -> String
remapEngine [] x = x
remapEngine ((l,r):xs) x = if l == x
  then r
  else remapEngine xs x

getText' :: String -> IBusH -> IO String
getText' _ (IBusH h m) = do
  engine <- try $engineName <$> getIBusEngine h
  case engine of
    (Left e) -> return (clientErrorMessage e)
    (Right x) -> return $remapEngine m x

getEventText' :: Fd -> String -> IBusH -> IO String
getEventText' fd _ (IBusH _ m) = do
-- The 'last . lines' part ensures we get the last event
-- If we didn't do this a rapid change in engines 
-- (two update events before we read once) could do weird stuff
  engine <- last . lines . fst <$> fdRead fd 512
  return $remapEngine m engine

