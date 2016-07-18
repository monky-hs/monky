{-
    Copyright 2015 Markus Ongyerth, Stephan Guenther

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
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.MPD
Description : Allows to query information from an mpd server
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module creates a persistant connection to an mpd server and allows to query
information or react to events waited for with MPDs idle
-}
module Monky.MPD
  ( MPDSocket
  , State(..)
  , TagCollection(..)
  , SongInfo(..)
  , Status(..)
  , getMPDStatus
  , getMPDSong
  , getMPDSocket
  , closeMPDSocket
  , getMPDFd
  , readOk
  , goIdle
  , doQuery -- This might not stay
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Read as R

import System.IO.Error
import GHC.IO.Exception
import Control.Exception (try)
import Control.Monad (join, void, unless)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (isJust,fromJust)
import System.Posix.Types (Fd(..))
import System.Timeout (timeout)
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

type MPDSock = Socket
-- |A type safe socket for this module, basically a handle
newtype MPDSocket = MPDSocket MPDSock

-- |The current state of the MPD player submodule
data State
  = Playing -- ^Playing
  | Stopped -- ^Stopped
  | Paused  -- ^Paused
  deriving (Show, Eq) -- If it wasn't obvious

-- |Collection of tags MPD supports
data TagCollection = TagCollection
  {
    tagArtist          :: Maybe Text
  , tagArtistSort      :: Maybe Text
  , tagAlbum           :: Maybe Text
  , tagAlbumSort       :: Maybe Text
  , tagAlbumArtist     :: Maybe Text
  , tagAlbumArtistSort :: Maybe Text
  , tagTitle           :: Maybe Text
  , tagTrack           :: Maybe Text
  , tagName            :: Maybe Text
  , tagGenre           :: Maybe Text
  , tagDate            :: Maybe Text
  , tagComposer        :: Maybe Text
  , tagPerformer       :: Maybe Text
  , tagComment         :: Maybe Text
  , tagDisc            :: Maybe Text
  , tagMArtistid       :: Maybe Text
  , tagMAlbumid        :: Maybe Text
  , tagMAlbumArtistid  :: Maybe Text
  , tagMTrackid        :: Maybe Text
  , tagMReleaseTrackid :: Maybe Text
  } deriving (Show, Eq)

-- |Information about an song
data SongInfo = SongInfo
  {
    songFile     :: Text
  , songRange    :: Maybe (Float, Float)
  , songMTime    :: Maybe Text
  , songTime     :: Maybe Int
  , songDuration :: Maybe Float
  , songTags     :: TagCollection
  , songPos      :: Maybe Int
  , songInfoId   :: Maybe Int
  , songPriority :: Maybe Int
  } deriving (Show, Eq)

-- |Haskell type for the data returned by MPDs status command
data Status = Status
  {  -- We get those values every time with current versions (maybe for outdated mpd)
    volume         :: Int
  , repeats        :: Bool
  , random         :: Bool
  , single         :: Maybe Bool --added with 0.15
  , consume        :: Maybe Bool --added with 0.15
  , playlist       :: Int
  , playlistLength :: Int
  , state          :: State
  , mixrAmpdb      :: Float

  -- Those values may be sent with the state for current versions
  , xfade          :: Maybe Int
  , mixrAmpDelay   :: Maybe Int
  , song           :: Maybe Int --playlist song number
  , songId         :: Maybe Int --playlist songid
  , nextSong       :: Maybe Int --added with 0.15
  , nextSongId     :: Maybe Int --added with 0.15
  , time           :: Maybe Int
  , elapsed        :: Maybe Float --added with 0.16
  , bitrate        :: Maybe Int
  , duration       :: Maybe Int --added with 0.20
  , audio          :: Maybe (Int,Int,Int)
  , updating       :: Maybe Int
  , mpderror       :: Maybe Text
  } deriving (Show, Eq)

-- This isn't nice, but OK for now
readText :: Integral a => Text -> a
readText t = let (Right (x, _)) = R.decimal t in x

rethrowSExcpt :: String -> IOError -> ExceptT String IO a
rethrowSExcpt xs e = throwE (xs ++ ": " ++ show e)


trySExcpt :: String -> IO a -> ExceptT String IO a
trySExcpt l f = liftIO (try f) >>= rethrow
  where rethrow (Left x) = rethrowSExcpt l x
        rethrow (Right x) = return x

-- |Close the 'MPDSocket' when it is no longer required
closeMPDSocket :: MPDSocket -> IO ()
closeMPDSocket (MPDSocket sock) = close sock

-- Is this connection exception recoverable or should we just die?
recoverableCon :: IOError -> Bool
recoverableCon x
  | ioeGetErrorType x == NoSuchThing = True
  | otherwise = False


tryConnect :: [AddrInfo] -> MPDSock -> ExceptT String IO MPDSock
tryConnect [] _ = error "Tryed to connect with non existing socket"
tryConnect (x:xs) sock =
  liftIO (try $connect sock (addrAddress x)) >>= handleExcpt
  where handleExcpt (Right _) = return sock
        handleExcpt (Left y) = if recoverableCon y
          then liftIO (close sock) >> openMPDSocket xs
          else rethrowSExcpt "Connect" y


doMPDConnInit :: MPDSock -> IO (Maybe String)
doMPDConnInit s = join <$> timeout (500 * 1000) doInit
  where
    doInit = do
      v <- BS.unpack <$> recv s 64
      if "OK MPD " `isPrefixOf` v
        then return . Just $drop 7 v
        else return Nothing


-- |Open one MPDSock connected to a host specified by one of the 'MPDInfo'
-- or throw an exception (either ran out of hosts or something underlying failed)
openMPDSocket :: [AddrInfo] -> ExceptT String IO MPDSock
openMPDSocket [] = throwE "Could not open connection"
openMPDSocket ys@(x:xs) = do
  sock <- trySExcpt "socket" $openSocket x
  csock <- tryConnect ys sock
  version <- trySExcpt "readInit" $doMPDConnInit csock
  if isJust version
    then liftIO $return csock
    else liftIO (close sock) >> openMPDSocket xs
  where openSocket y = socket (addrFamily y) (addrSocketType y) (addrProtocol y)


-- |Get a new 'MPDSocket'
getMPDSocket
  :: String -- ^The host the server is on
  -> String -- ^The port the server listens on
  -> IO (Either String MPDSocket)
getMPDSocket host port = do
  xs <- getAddrInfo (Just $defaultHints {addrFlags = [AI_V4MAPPED]}) (Just host) (Just port)
  sock <- runExceptT $openMPDSocket xs
  return $fmap MPDSocket sock

-- |Get the raw 'Fd' from the 'MPDSocket' for eventing api
getMPDFd :: MPDSocket -> IO Fd
getMPDFd (MPDSocket s) = return . Fd $fdSocket s

recvMessage :: MPDSock -> ExceptT String IO [Text]
recvMessage sock =
  trySExcpt "receive" $ T.lines . E.decodeUtf8 <$> recv sock 4096

sendMessage :: MPDSock -> String -> ExceptT String IO ()
sendMessage sock message = void $
  trySExcpt "send" (sendAll sock $BS.pack message)

{- |Send a query to the MPD server and return a list of answers (line by line)

This does not filter out errors, error checking has to be done by the user.
This does filter out the last OK though
-}

doQuery :: MPDSocket -> String -> ExceptT String IO [Text]
doQuery (MPDSocket s) m = sendMessage s (m ++ "\n") >> (f <$> recvMessage s)
  where f = filter (/= "OK")

-- |Get the OK sent by mpd after idle out of the pipe
readOk :: MPDSocket -> IO (Either String ())
readOk (MPDSocket s) = runExceptT $ do
  resp <- recvMessage s
  unless (resp == ["OK"]) (throwE . T.unpack . T.concat $ resp)

{- |Go into MPDs 'idle' mode, this does return, but MPD wont time us out and
will notify us if something happens
-}
goIdle 
  :: MPDSocket 
  -> String -- ^The string that should be send with idle (list of subsystems)
  -> IO (Either String ())
goIdle (MPDSocket s) xs = runExceptT $ void $ sendMessage s ("idle" ++ xs ++ "\n")


getAudioTuple :: Text -> (Int,Int,Int)
getAudioTuple xs =
  let (Right (x, ys)) = R.decimal xs
      (Right (y, zs)) = R.decimal $ T.tail ys
      (Right (z, _))  = R.decimal $ T.tail zs
    in (x, y, z)


getState :: Text -> State
getState "play"  = Playing
getState "stop"  = Stopped
getState "pause" = Paused
getState xs = error ("Got unknown state: " ++ T.unpack xs)


parseStatusRec :: M.Map Text Text -> [Text] -> Status
parseStatusRec m (x:xs) = let (key,value) = T.break (== ' ') x in
  parseStatusRec (M.insert (T.init key) (T.tail value) m) xs
parseStatusRec m [] = Status
  { volume = fromJust $getInt "volume"
  , repeats = fromJust $getBool "repeat"
  , random = fromJust $getBool "random"
  , single = getBool "single"
  , consume = getBool "consume"
  , playlist = fromJust $getInt "playlist"
  , playlistLength = fromJust $getInt "playlistlength"
  , state = getState_ m
  , song = getInt "song"
  , songId = getInt "songid"
  , nextSong = getInt "nextsong"
  , nextSongId = getInt "nextsongid"
  , time = fmap (readText . T.takeWhile (/= ':')) $getVal "time"
  , elapsed = read . T.unpack <$> M.lookup "elapsed" m
  , duration = getInt "duration"
  , bitrate = getInt "bitrate"
  , xfade = getInt "xfade"
  , mixrAmpdb = fromJust . fmap readF $ getVal "mixrampdb"
  , mixrAmpDelay = getInt "mixrampdelay"
  , audio = getAudio m
  , updating = getInt "updating_db"
  , mpderror = M.lookup "error" m
  }
  where getVal = flip M.lookup m
        getBool = fmap (== ("1" :: Text)) . getVal
        getInt = fmap readText . getVal
        getState_ = getState . fromJust . M.lookup "state"
        getAudio = fmap getAudioTuple . M.lookup "audio"
        readF t = let (Right (x, _)) = R.rational t in x


parseSongInfoRec :: M.Map Text Text -> [Text] -> SongInfo
-- use init to drop the ':' for keys
-- use break instead of words because of comment or artist
parseSongInfoRec m (x:xs) = let (key, value) = T.break isSpace x in
  parseSongInfoRec (M.insert (T.init key) (T.tail value) m) xs
parseSongInfoRec m [] = let tags = TagCollection {
    tagArtist = M.lookup "Artist" m
  , tagArtistSort = M.lookup "ArtistSort" m
  , tagAlbum = M.lookup "Album" m
  , tagAlbumSort = M.lookup "AlbumSort" m
  , tagAlbumArtist = M.lookup "AlbumArtist" m
  , tagAlbumArtistSort = M.lookup "AlbumArtistSort" m
  , tagTitle = M.lookup "Title" m
  , tagTrack = M.lookup "Track" m
  , tagName = M.lookup "Name" m
  , tagGenre = M.lookup "Genre" m
  , tagDate = M.lookup "Date" m
  , tagComposer = M.lookup "Composer" m
  , tagPerformer = M.lookup "Performer" m
  , tagComment = M.lookup "Comment" m
  , tagDisc = M.lookup "Disc" m

  , tagMArtistid = M.lookup "MUSICBRAINZ_ARTISTID" m
  , tagMAlbumid = M.lookup "MUSICBRAINZ_ALBUMID" m
  , tagMAlbumArtistid = M.lookup "MUSICBRAINZ_ALBUMARTISTID" m
  , tagMTrackid = M.lookup "MUSICBRAINZ_TRACKID" m
  , tagMReleaseTrackid = M.lookup "MUSICBRAINZ_RELEASETRACKID" m
  } in
    SongInfo {
      songFile = fromJust $M.lookup "file" m
    , songRange = readT <$> M.lookup "Range" m
    , songMTime = M.lookup "Last-Modified" m
    , songTime = mRead "Time"
    , songDuration = readF <$> M.lookup "duration" m
    , songTags = tags
    , songPos = mRead "Pos"
    , songInfoId = mRead "Id"
    , songPriority = mRead "Prio"
    }
  where readT xs =
          let (Right (x, ys)) = R.rational xs
              (Right (y, _))  = R.rational . T.tail $ ys in
            (x, y)
        mRead = fmap (readText) . flip M.lookup m
        readF t = let (Right (x, _)) = R.rational t in x


parseStatus :: [Text] -> Status
parseStatus [] = error "Called parseStatus with []"
parseStatus xs@(x:_) = if "ACK" `T.isPrefixOf` x
  then error (T.unpack x)
  else parseStatusRec M.empty xs

-- |Get the current status or an error from the MPD server
getMPDStatus :: MPDSocket -> IO (Either String Status)
getMPDStatus s = do
  resp <- runExceptT $doQuery s "status"
  return $fmap parseStatus resp

parseSongInfo :: [Text] -> SongInfo
parseSongInfo [] = error "Called parseSongInfo with []"
parseSongInfo xs@(x:_) = if "ACK" `T.isPrefixOf` x
  then error (T.unpack x)
  else parseSongInfoRec M.empty xs

-- |Get the information about the song currently beeing player from the MPD server
getMPDSong :: MPDSocket -> IO (Either String SongInfo)
getMPDSong s = do
  resp <- runExceptT $doQuery s "currentsong"
  return $fmap parseSongInfo resp
