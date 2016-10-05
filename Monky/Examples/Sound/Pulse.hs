{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
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

data PulseH = PulseH (Maybe String)

printSink :: ([MonkyOut] -> IO ()) -> Sinkinfo -> IO ()
printSink fun sink = do
    let vol = cVolumeToLinear $ siVolume sink
    let baseV = volumeToLinear $ siBaseVolume sink
    let mute = siMute sink
    let rvols =  map (\v -> v / baseV * 100) vol
    let rvol = sum rvols / genericLength rvols
    let str = if mute
        then "Mute"
        else sformat ((left 3 ' ' %. int) % "%") (round rvol :: Int)
    fun [MonkyPlain str]


startLoop :: Context -> ([MonkyOut] -> IO ()) -> Sinkinfo -> IO ()
startLoop cxt cfun info = do 
    void $ subscribeEvents cxt [SubscriptionMaskSink] fun
    printSink cfun info
    where fun :: ((SubscriptionEventFacility, SubscriptionEventType) -> Word32 -> IO ())
          fun _ 0 = void $ getContextSinkByIndex cxt (siIndex info) (printSink cfun)
          fun _ _ = return ()


startWithName :: Context -> String -> ([MonkyOut] -> IO ()) -> IO ()
startWithName cxt name fun = void $ getContextSinkByName cxt name (startLoop cxt fun)


getDefaultSink :: Context -> ([MonkyOut] -> IO ()) -> IO ()
getDefaultSink cxt cfun = void $ getServerInfo cxt fun
    where fun :: ServerInfo -> IO ()
          fun serv = let name = defaultSinkName serv
                     in startWithName cxt name cfun


startPulse :: Maybe String -> ([MonkyOut] -> IO ()) -> IO ()
startPulse = startPulseMaybe (return ())


startPulseMaybe
    :: IO () -- ^The continuation if everything fails
    -> Maybe String -- ^Sink to use (Nothing for default)
    -> ([MonkyOut] -> IO ()) -- ^Output function
    -> IO ()
startPulseMaybe ret sinkName fun = do
    impl <- getMainloopImpl
    cxt <- getContext impl "monky" -- should we get the exe name?
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

instance PollModule PulseH where
    initialize (PulseH _) = do
        running <- startSyncPulse
        if running
           then return ()
           else return ()
    getOutput _ = return []

getPulseHandle :: Maybe String -> IO PulseH
getPulseHandle = return . PulseH
