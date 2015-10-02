{-# LANGUAGE OverloadedStrings #-}
module Monky.MPD
(MPDSocket, State(..), TagCollection(..), SongInfo(..), Status(..),
 getMPDStatus, getMPDSong, getMPDSocket,
 doQuery -- This might not stay
 )
where

import System.Socket
import Data.Maybe (isJust,fromJust)
import qualified Data.ByteString.Char8 as BS (unpack,pack)

import Control.Applicative ((<$>))
import Data.List (isPrefixOf)

import qualified Data.Map as M

type MPDSock = Socket Inet6 Stream TCP
data MPDSocket = MPDSocket MPDSock
type MPDInfo = AddressInfo Inet6 Stream TCP

data Response = Err String | Success [String] deriving (Show,Eq)

data State = Playing | Stopped | Paused deriving (Show,Eq)

data TagCollection = TagCollection
  {
    tagArtist          :: Maybe String
  , tagArtistSort      :: Maybe String
  , tagAlbum           :: Maybe String
  , tagAlbumSort       :: Maybe String
  , tagAlbumArtist     :: Maybe String
  , tagAlbumArtistSort :: Maybe String
  , tagTitle           :: Maybe String
  , tagTrack           :: Maybe String
  , tagName            :: Maybe String
  , tagGenre           :: Maybe String
  , tagDate            :: Maybe String -- TODO this should parse as date
  , tagComposer        :: Maybe String
  , tagPerformer       :: Maybe String
  , tagComment         :: Maybe String
  , tagDisc            :: Maybe String
  , tagMArtistid       :: Maybe String
  , tagMAlbumid        :: Maybe String
  , tagMAlbumArtistid  :: Maybe String
  , tagMTrackid        :: Maybe String
  , tagMReleaseTrackid :: Maybe String
  } deriving (Show, Eq)

data SongInfo = SongInfo
  {
    songFile     :: String
  , songRange    :: Maybe (Float, Float)
  , songMTime    :: Maybe String -- time_print
  , songTime     :: Maybe Int
  , songDuration :: Maybe Float
  , songTags     :: TagCollection
  , songPos      :: Maybe Int
  , songInfoId   :: Maybe Int
  , songPriority :: Maybe Int
  } deriving (Show, Eq)

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
  , elapsed        :: Maybe Float --added with [3]
  , bitrate        :: Maybe Int
  , duration       :: Maybe Int --added with [4]
  , audio          :: Maybe (Int,Int,Int)
  , updating       :: Maybe Int
  , mpderror       :: Maybe String
  } deriving (Show, Eq)

doMPDConnInit :: MPDSocket -> IO (Maybe String)
doMPDConnInit (MPDSocket s) = do
  v <- BS.unpack <$> receive s 64 (MessageFlags 0)
  if "OK MPD " `isPrefixOf` v
    then return . Just $drop 7 v
    else return Nothing

-- This would be the place to do version checking for mpd protocol
openMPDSocket :: [MPDInfo] -> IO MPDSocket
openMPDSocket [] = error "Could not open connection"
openMPDSocket (x:xs) = do
  sock <- socket :: IO (Socket Inet6 Stream TCP)
  connect sock $socketAddress x
  let msock = MPDSocket sock
  version <- doMPDConnInit msock
  if isJust version
    then return msock
    else close sock >> openMPDSocket xs

getMPDSocket :: String -> String -> IO MPDSocket
getMPDSocket host port = do
  xs <- getAddressInfo (Just $BS.pack host) (Just $BS.pack port) aiV4Mapped
  openMPDSocket xs


recvMessage :: MPDSock -> IO Response
recvMessage sock = do
  message <- BS.unpack <$> receive sock 4096 (MessageFlags 0)
  if "ACK" `isPrefixOf` message
    then return . Err $drop 4 message
    else return . Success . init $lines message

sendMessage :: MPDSock -> String -> IO ()
sendMessage sock message = do
  ret <- send sock (BS.pack message) (MessageFlags 0)
  if ret /= length message
    then error "Could not send whole message"
    else return ()

doQuery :: MPDSocket -> String -> IO Response
doQuery (MPDSocket s) m = sendMessage s (m ++ "\n") >> recvMessage s

-- TODO this is silly
getAudioTuple :: String -> (Int,Int,Int)
getAudioTuple xs = let
  (x,':':ys) = break (== ':') xs
  (y,':':z)  = break (== ':') ys in
    (read x, read y, read z)

getState :: String -> State
getState "play"  = Playing
getState "stop"  = Stopped
getState "pause" = Paused
getState _ = error "Got unknown state"

parseStatusRec :: M.Map String String -> [String] -> Status
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
  , time = fmap (read . takeWhile (/= ':')) $getVal "time"
  , elapsed = fmap read $M.lookup "elapsed" m
  , duration = getInt "duration"
  , bitrate = getInt "bitrate"
  , xfade = getInt "xfade"
  , mixrAmpdb = fromJust . fmap read $getVal "mixrampdb"
  , mixrAmpDelay = getInt "mixrampdelay"
  , audio = getAudio m
  , updating = getInt "updating_db"
  , mpderror = M.lookup "error" m
  }
  where getVal = flip M.lookup m
        getBool = fmap (=="1") . getVal
        getInt = fmap read . getVal
        getState_ = getState . fromJust . M.lookup "state"
        getAudio = fmap getAudioTuple . M.lookup "audio"
-- use init to drop the ':' for keys
parseStatusRec m (x:xs) = let [key,value] = words x in
  parseStatusRec (M.insert (init key) value m) xs

parseStatus :: [String] -> Status
parseStatus = parseStatusRec M.empty

parseSongInfoRec :: M.Map String String -> [String] -> SongInfo
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
    , songRange = fmap (readT . split '-') $M.lookup "Range" m
    , songMTime = M.lookup "Last-Modified" m
    , songTime = fmap read $M.lookup "Time" m
    , songDuration = fmap read $M.lookup "duration" m
    , songTags = tags
    , songPos = fmap read $M.lookup "Pos" m
    , songInfoId = fmap read $M.lookup "Id" m
    , songPriority = fmap read $M.lookup "Prio" m
    }
  where split x xs = (takeWhile (/=x) xs, tail $dropWhile (/=x) xs)
        readT (x,y) = (read x, read y)
-- use init to drop the ':' for keys
-- use break instead of words because of comment or artist
parseSongInfoRec m (x:xs) = let (key,' ':value) = break (==' ') x in
  parseSongInfoRec (M.insert (init key) value m) xs

parseSongInfo :: [String] -> SongInfo
parseSongInfo = parseSongInfoRec M.empty


getMPDStatus :: MPDSocket -> IO Status
getMPDStatus s = do
  resp <- doQuery s "status"
  return $parseResp resp
  where parseResp (Success xs) = parseStatus xs
        parseResp (Err x) = error x

getMPDSong :: MPDSocket -> IO SongInfo
getMPDSong s = do
  resp <- doQuery s "currentsong"
  return $parseResp resp
  where parseResp (Success xs) = parseSongInfo xs
        parseResp (Err x) = error x
