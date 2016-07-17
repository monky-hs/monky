{-|
Module      : Monky.Examples.MPD
Description : An example module instance for the MPD module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Monky.Examples.MPD
  ( MPDHandle
  , getMPDHandle
  )
where

import qualified Data.Text as T

import Data.IORef
import Data.Maybe (fromMaybe)
import System.IO (hPutStrLn, stderr)
import System.Posix.Types (Fd)

import Monky.MPD
import Monky.Modules
import Monky.Examples.Utility

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

ioInM :: (a -> IO b) -> Maybe a -> IO (Maybe b)
ioInM _ Nothing = return Nothing
ioInM act (Just x) = do
  ret <- act x
  return (Just ret)


getPlayingSong :: State -> MPDSocket -> IO (Either String SongInfo)
getPlayingSong Playing s = getMPDSong s
getPlayingSong _ _ = return (Left "Not playing")


extractTitle :: SongInfo -> Maybe String
extractTitle = tagTitle . songTags


getSongTitle :: MPDSocket -> IO String
getSongTitle sock = getMPDStatus sock >>= getSong
  where getSong (Left x) = return x
        getSong (Right status) = getTitle <$> getPlayingSong (state status) sock
        getTitle (Left x) = x
        getTitle (Right x) = fromMaybe "No Title" $extractTitle x


-- |The handle for this example
data MPDHandle = MPDHandle String String (IORef (Maybe MPDSocket))


-- TODO ignoring errors is never a good idea
getEvent :: MPDSocket -> IO String
getEvent s = do
  _ <- readOk s
  t <- getSongTitle s
  _ <- goIdle s " player"
  return t


getFd :: MPDSocket -> IO [Fd]
getFd s = do
  fd <- getMPDFd s
  _ <- goIdle s " player"
  return [fd]


instance Module MPDHandle where
-- The unfailable ones aren't called if failable is defined explicitly
  getEventText _ _ _ = return ""
  getText _ _ = return ""
  getTextFailable _ (MPDHandle _ _ s) = do
    r <- readIORef s
    ioInM getSongTitle r
  getEventTextFailable _ _ (MPDHandle _ _ s) =  do
    r <- readIORef s
    ioInM getEvent r
  recoverModule (MPDHandle h p r) = do
    s <- getMPDSocket h p
    case s of
      (Right x) -> writeIORef r (Just x) >> return True
      (Left _) -> return False
  getFDs (MPDHandle _ _ s) = do
    r <- readIORef s
    case r of
      Nothing -> return []
      Just x -> getFd x


instance NewModule MPDHandle where
  getOutput (MPDHandle _ _ s) = do
    r <- readIORef s
    case r of
      Nothing -> return [MonkyPlain "Broken"]
      (Just x) -> do
        ret <- getSongTitle x
        return [MonkyPlain . T.pack $ ret]
  initialize (MPDHandle h p r) = do
    s <- getMPDSocket h p
    case s of
      (Right x) -> writeIORef r (Just x)
      (Left _) -> return ()


instance EvtModule MPDHandle where
  startEvtLoop h@(MPDHandle _ _ s) ref = do
    initialize h
    c <- getOutput h
    atomicWriteIORef ref c
    r <- readIORef s
    case r of
      Nothing -> hPutStrLn stderr "Could not initialize MPDHandle :("
      (Just x) -> do
        [fd] <- getFd x
        loopFd x fd ref (fmap (\y -> [MonkyPlain . T.pack $ y]) . getEvent)


-- |Get an 'MPDHandle' (server has to be running when this is executed)
getMPDHandle
  :: String -- ^The host to connect to
 -> String  -- ^The port to connect to
 -> IO MPDHandle
getMPDHandle h p = MPDHandle h p <$> newIORef Nothing

