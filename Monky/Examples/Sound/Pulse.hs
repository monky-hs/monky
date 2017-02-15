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
{-# LANGUAGE CPP #-}
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
import Control.Monad (void)
import Control.Monad.IO.Class

import System.Environment (getProgName)

import Sound.Pulse
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

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>), (<*>))
#endif

-- |Pulse handle.
-- This module uses continuations, the handle is not really used
data PulseH = PulseH (Maybe String)

-- |Format a given sink to the output string.
printSink :: ([MonkyOut] -> IO ()) -> Sinkinfo -> IO ()
printSink fun sink = do
    let norm = 0x10000 :: Double
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

-- | Find the index of our sink and register the event handler
startLoopM :: Maybe String -> ([MonkyOut] -> IO ()) -> Pulse ()
startLoopM str fun = do
    name <- maybe (defaultSinkName <$> getServerInfoM) return str
    sink <- getContextSinkByNameM name
    void $ subscribeEventsM [SubscriptionMaskSink] (sub $ siIndex sink)
    liftIO $ printSink fun sink
    where sub :: Word32 -> Context -> ((SubscriptionEventFacility, SubscriptionEventType) -> Word32 -> IO ())
          sub i cxt _ 0 = void $ getContextSinkByIndex cxt i (printSink fun)
          sub _ _   _ _ = return ()

-- | Try to start pulse loop. silently fail!
startPulse :: Maybe String -> ([MonkyOut] -> IO ()) -> IO ()
startPulse = startPulseMaybe (return ())

-- | Connect to pulse server and try to start the main loop for this module.
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
            ContextReady ->
                runPulse_ cxt (startLoopM sinkName fun)
            _ -> return ()
    _ <- connectContext cxt Nothing []
    _ <- doLoop impl
    ret

instance EvtModule PulseH where
    startEvtLoop (PulseH server) fun = startPulseMaybe (fun [MonkyPlain "None"]) server fun


--startSyncPulse
--    :: IO Bool
--startSyncPulse = do
--    impl <- getMainloopImpl
--    cxt <- getContext impl "monky" -- should we get the exe name?
--    var <- newEmptyMVar
--    setStateCallback cxt $ do
--        state <- getContextState cxt
--        case state of
--            ContextFailed -> do
--                hPutStrLn stderr "Could not connect to pulse :("
--                quitLoop impl (-1)
--                putMVar var False
--            ContextReady ->
--                putMVar var True
--            _ -> return ()
--    _ <- connectContext cxt Nothing []
--    _ <- forkIO . void $ doLoop impl
--    -- Should we care?
--    readMVar var

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
