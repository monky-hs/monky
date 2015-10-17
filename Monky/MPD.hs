{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Monky.MPD
(MPDSocket, State(..), TagCollection(..), SongInfo(..), Status(..),
 getMPDStatus, getMPDSong, getMPDSocket, closeMPDSocket, getMPDFd,
 readOk, goIdle,
 doQuery -- This might not stay
 )
where

import System.IO.Error
import GHC.IO.Exception
import Control.Exception (try)
import Control.Monad (join)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (isJust,fromJust)
import Monky.Utility (splitAtEvery)
import System.Posix.Types (Fd(..))
import System.Timeout (timeout)
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as BS (unpack,pack)
import qualified Data.Map as M

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

type MPDSock = Socket
data MPDSocket = MPDSocket MPDSock

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
  , tagDate            :: Maybe String
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
  , songMTime    :: Maybe String
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
  , elapsed        :: Maybe Float --added with 0.16
  , bitrate        :: Maybe Int
  , duration       :: Maybe Int --added with 0.20
  , audio          :: Maybe (Int,Int,Int)
  , updating       :: Maybe Int
  , mpderror       :: Maybe String
  } deriving (Show, Eq)


rethrowSExcpt :: String -> IOError -> ExceptT String IO a
rethrowSExcpt xs e = throwE (xs ++ ": " ++ show e)


trySExcpt :: String -> IO a -> ExceptT String IO a
trySExcpt l f = rethrow =<< (liftIO $try f)
  where rethrow (Left x) = rethrowSExcpt l x
        rethrow (Right x) = return x

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
doMPDConnInit s = fmap join $timeout 500000 $do
  v <- BS.unpack <$> recv s 64
  if "OK MPD " `isPrefixOf` v
    then return . Just $drop 7 v
    else return Nothing


-- |Open one 'MPDSock' connected to a host specified by one of the 'MPDInfo'
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


getMPDSocket :: String -> String -> IO (Either String MPDSocket)
getMPDSocket host port = do
  xs <- getAddrInfo (Just $defaultHints {addrFlags = [AI_V4MAPPED]}) (Just host) (Just port)
  sock <- runExceptT $openMPDSocket xs
  return $fmap MPDSocket sock

getMPDFd :: MPDSocket -> IO Fd
getMPDFd (MPDSocket s) = return . Fd $fdSocket s

recvMessage :: MPDSock -> ExceptT String IO [String]
recvMessage sock = 
  trySExcpt "receive" $lines . BS.unpack <$> recv sock 4096

sendMessage :: MPDSock -> String -> ExceptT String IO ()
sendMessage sock message =
  trySExcpt "send" (sendAll sock $BS.pack message) >> return ()

doQuery :: MPDSocket -> String -> ExceptT String IO [String]
doQuery (MPDSocket s) m = sendMessage s (m ++ "\n") >> (f <$> recvMessage s)
  where f = filter (/= "OK")

readOk :: MPDSocket -> IO (Either String ())
readOk (MPDSocket s) = runExceptT $ do
  resp <- recvMessage s
  if resp == ["OK"]
    then return ()
    else throwE $concat resp

goIdle :: MPDSocket -> String -> IO (Either String ())
goIdle (MPDSocket s) xs = runExceptT (sendMessage s ("idle" ++ xs ++ "\n") >> return ())


getAudioTuple :: String -> (Int,Int,Int)
getAudioTuple xs = let [x,y,z] = read <$> splitAtEvery ":" xs in (x,y,z)


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
parseStatusRec m (x:xs) = let (key,' ':value) = break (== ' ') x in
  parseStatusRec (M.insert (init key) value m) xs


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
parseSongInfoRec m (x:xs) = let (key,' ':value) = break (isSpace) x in
  parseSongInfoRec (M.insert (init key) value m) xs


parseStatus :: [String] -> Status
parseStatus [] = error "Called parseStatus with []"
parseStatus xs@(x:_) = if "ACK" `isPrefixOf` x
  then error x
  else parseStatusRec M.empty xs

getMPDStatus :: MPDSocket -> IO (Either String Status)
getMPDStatus s = do
  resp <- runExceptT $doQuery s "status"
  return $fmap parseStatus resp



parseSongInfo :: [String] -> SongInfo
parseSongInfo [] = error "Called parseSongInfo with []"
parseSongInfo xs@(x:_) = if "ACK" `isPrefixOf` x
  then error x
  else parseSongInfoRec M.empty xs

getMPDSong :: MPDSocket -> IO (Either String SongInfo)
getMPDSong s = do
  resp <- runExceptT $doQuery s "currentsong"
  return $fmap parseSongInfo resp
