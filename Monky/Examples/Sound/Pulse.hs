{-
    Copyright 2016 Markus Ongyerth

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
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.Sound.Pulse
Description : Integration example for pulseaudio library.
Maintainer  : ongy
Stability   : experimental
Portability : Linux

Some of this may move into Monky.Sound.Pulse in a near-ish future.
-}
module Monky.Examples.Sound.Pulse
    ( PulseH

    , startPulse
    , startPulseMaybe
    , getPulseHandle
    )
where

import Monky.Modules
import Data.Maybe (fromMaybe)
import Control.Monad (void)
import Control.Concurrent.MVar
import Control.Concurrent

import System.Environment (getProgName)

import Sound.Pulse.Context
import Sound.Pulse.Mainloop
import Sound.Pulse.Mainloop.Simple

import Sound.Pulse.Volume
import Sound.Pulse.Subscribe
import Sound.Pulse.Serverinfo
import Sound.Pulse.Sinkinfo

import System.IO (hPutStrLn, stderr)
import Data.Word (Word32)
import Data.List (genericLength)
import Formatting

-- |Pulse handle.
-- This module uses continuations, the handle is not really used
data PulseH = PulseH (Maybe String)

-- |Format a given sink to the output string.
printSink :: ([MonkyOut] -> IO ()) -> Sinkinfo -> IO ()
printSink fun sink = do
    let norm = 0x10000
    let (CVolume vol) = siVolume sink
    let mute = siMute sink
    -- this is taken from pulseaudio pa_volume_snprint, +- rounding this is
    -- equal to pavucontrol
    let rvols =  map (\v -> (fromIntegral v*100+norm/2) / norm) vol
    let rvol = sum rvols / genericLength rvols
    let str = if mute
        then "Mute"
        else sformat ((left 3 ' ' %. int) % "%") (round rvol :: Int)
    fun [MonkyPlain str]

-- |Start the subscription loop that will give us the updates.
startLoop :: Context -> ([MonkyOut] -> IO ()) -> Sinkinfo -> IO ()
startLoop cxt cfun info = do 
    void $ subscribeEvents cxt [SubscriptionMaskSink] fun
    printSink cfun info
    where fun :: ((SubscriptionEventFacility, SubscriptionEventType) -> Word32 -> IO ())
          fun _ 0 = void $ getContextSinkByIndex cxt (siIndex info) (printSink cfun)
          fun _ _ = return ()

-- |Use the name (string) to find the index, print once and find the main loop
startWithName :: Context -> String -> ([MonkyOut] -> IO ()) -> IO ()
startWithName cxt name fun = void $ getContextSinkByName cxt name (startLoop cxt fun)

-- |Get the default sink and start the main loop
getDefaultSink :: Context -> ([MonkyOut] -> IO ()) -> IO ()
getDefaultSink cxt cfun = void $ getServerInfo cxt fun
    where fun :: ServerInfo -> IO ()
          fun serv = let name = defaultSinkName serv
                     in startWithName cxt name cfun

-- |Try to start pulse loop. silently fail!
startPulse :: Maybe String -> ([MonkyOut] -> IO ()) -> IO ()
startPulse = startPulseMaybe (return ())


-- |Connect to pulse server and try to start the main loop for this module.
startPulseMaybe
    :: IO () -- ^The continuation if everything fails
    -> Maybe String -- ^Sink to use (Nothing for default)
    -> ([MonkyOut] -> IO ()) -- ^Output function
    -> IO ()
startPulseMaybe ret sinkName fun = do
    impl <- getMainloopImpl
    name <- getProgName
    cxt <- getContext impl name
    setStateCallback cxt $ do
        state <- getContextState cxt
        case state of
            ContextFailed -> do
                hPutStrLn stderr "Could not connect to pulse :("
                quitLoop impl (-1)
            ContextReady -> fromMaybe
                (getDefaultSink cxt fun )
                (startWithName cxt <$> sinkName <*> return fun)
            _ -> return ()
    _ <- connectContext cxt Nothing []
    _ <- doLoop impl
    ret

instance EvtModule PulseH where
    startEvtLoop (PulseH server) fun = startPulseMaybe (fun [MonkyPlain "None"]) server fun


startSyncPulse
    :: IO Bool
startSyncPulse = do
    impl <- getMainloopImpl
    cxt <- getContext impl "monky" -- should we get the exe name?
    var <- newEmptyMVar
    setStateCallback cxt $ do
        state <- getContextState cxt
        case state of
            ContextFailed -> do
                hPutStrLn stderr "Could not connect to pulse :("
                quitLoop impl (-1)
                putMVar var False
            ContextReady ->
                putMVar var True
            _ -> return ()
    _ <- connectContext cxt Nothing []
    _ <- forkIO . void $ doLoop impl
    -- Should we care?
    readMVar var

-- This (most of it) should move into pulseaudio package, not doing that yet.
-- instance PollModule PulseH where
--     initialize (PulseH _) = do
--         running <- startSyncPulse
--         if running
--            then return ()
--            else return ()
--     getOutput _ = return []

-- |Get the handle for pulse integration.
getPulseHandle
    :: Maybe String -- ^The servername (Nothing will use environment variable)
    -> IO PulseH
getPulseHandle = return . PulseH
