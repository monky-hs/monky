{-
    Copyright 2015-2017 Markus Ongyerth

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
{-# LANGUAGE OverloadedStrings #-}
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
  , getMPDHandleF
  )
where

import Data.Text (Text)
import qualified Data.Text as T

import Data.IORef
import System.IO (hPutStrLn, stderr)
import System.Posix.Types (Fd)

import Monky.MPD
import Monky.Modules
import Monky.Examples.Utility

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>), pure, (<*>))
#endif

type ConvertFun = (State, Maybe SongInfo) -> Text

getSongTitle :: MPDSocket -> ConvertFun -> IO Text
-- TODO: Clean this up a bit. Probably do notation?
getSongTitle sock fun = (fmap state <$> getMPDStatus sock) >>= getSong
  where getSong (Left x) = return . T.pack $ x
        getSong (Right Playing) = do
            info <- getMPDSong sock
            case info of
                Right x -> pure $ fun (Playing, Just x)
                Left x -> pure $ T.pack x
        getSong (Right x) = pure $ fun (x, Nothing)


-- |The handle for this example
data MPDHandle = MPDHandle
    { _host :: String
    , _port :: String
    , _sock :: IORef (Maybe MPDSocket)
    , _convert :: ConvertFun
    }


-- TODO ignoring errors is never a good idea
getEvent :: MPDSocket -> ConvertFun -> IO Text
getEvent s fun = do
  _ <- readOk s
  t <- getSongTitle s fun
  _ <- goIdle s " player"
  return t


getFd :: MPDSocket -> IO [Fd]
getFd s = do
  fd <- getMPDFd s
  _ <- goIdle s " player"
  return [fd]


instance PollModule MPDHandle where
  getOutput (MPDHandle _ _ s f) = do
    r <- readIORef s
    case r of
      Nothing -> return [MonkyPlain "Broken"]
      (Just x) -> do
        ret <- getSongTitle x f
        return [MonkyPlain ret]
  initialize (MPDHandle h p r _) = do
    s <- getMPDSocket h p
    case s of
      (Right x) -> writeIORef r (Just x)
      (Left _) -> return ()


instance EvtModule MPDHandle where
  startEvtLoop h@(MPDHandle _ _ s f) fun = do
    initialize h
    fun =<< getOutput h
    r <- readIORef s
    case r of
      Nothing -> hPutStrLn stderr "Could not initialize MPDHandle :("
      (Just x) -> do
        [fd] <- getFd x
        loopFd x fd fun (fmap (\y -> [MonkyPlain y]) . flip getEvent f)

defaultConvert :: (State, Maybe SongInfo) -> Text
defaultConvert (Playing, Just x) = case tagTitle . songTags $ x of
    Nothing -> "Can't extract song title"
    Just y -> y
defaultConvert (Playing, Nothing) = "Can't extract song"
defaultConvert _ = "Not Playing"

-- |Get an 'MPDHandle' (server has to be running when this is executed)
getMPDHandle
  :: String -- ^The host to connect to
  -> String  -- ^The port to connect to
  -> IO MPDHandle
getMPDHandle h p =
    MPDHandle h p <$> newIORef Nothing <*> pure defaultConvert

-- | Get the 'MPDHandle' with a custom conversion function. You will need to
-- import `Monky.MPD` to get the definitions into scope
getMPDHandleF
    :: String -- ^The host to connect to
    -> String -- ^The port to connect to
    -> ConvertFun -- ^The function to extract the text
    -> IO MPDHandle
getMPDHandleF h p f =
    MPDHandle h p <$> newIORef Nothing <*> pure f
