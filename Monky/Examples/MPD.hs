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

import Data.Text (Text)
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

getPlayingSong :: State -> MPDSocket -> IO (Either String SongInfo)
getPlayingSong Playing s = getMPDSong s
getPlayingSong _ _ = return (Left "Not playing")


extractTitle :: SongInfo -> Maybe Text
extractTitle = tagTitle . songTags


getSongTitle :: MPDSocket -> IO Text
getSongTitle sock = getMPDStatus sock >>= getSong
  where getSong (Left x) = return . T.pack $ x
        getSong (Right status) = getTitle <$> getPlayingSong (state status) sock
        getTitle (Left x) = T.pack x
        getTitle (Right x) = fromMaybe "No Title" $extractTitle x


-- |The handle for this example
data MPDHandle = MPDHandle String String (IORef (Maybe MPDSocket))


-- TODO ignoring errors is never a good idea
getEvent :: MPDSocket -> IO Text
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


instance PollModule MPDHandle where
  getOutput (MPDHandle _ _ s) = do
    r <- readIORef s
    case r of
      Nothing -> return [MonkyPlain "Broken"]
      (Just x) -> do
        ret <- getSongTitle x
        return [MonkyPlain ret]
  initialize (MPDHandle h p r) = do
    s <- getMPDSocket h p
    case s of
      (Right x) -> writeIORef r (Just x)
      (Left _) -> return ()


instance EvtModule MPDHandle where
  startEvtLoop h@(MPDHandle _ _ s) fun = do
    initialize h
    fun =<< getOutput h
    r <- readIORef s
    case r of
      Nothing -> hPutStrLn stderr "Could not initialize MPDHandle :("
      (Just x) -> do
        [fd] <- getFd x
        loopFd x fd fun (fmap (\y -> [MonkyPlain y]) . getEvent)


-- |Get an 'MPDHandle' (server has to be running when this is executed)
getMPDHandle
  :: String -- ^The host to connect to
 -> String  -- ^The port to connect to
 -> IO MPDHandle
getMPDHandle h p = MPDHandle h p <$> newIORef Nothing

