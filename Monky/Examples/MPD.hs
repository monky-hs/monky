{-# LANGUAGE StandaloneDeriving #-}
module Monky.Examples.MPD
(MPDHandle, getMPDHandle, getMPDHandle')
where


import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Monky.Modules
import Monky.MPD


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


data MPDHandle = MPDHandle MPDSocket

instance Module MPDHandle where
  getText _ (MPDHandle s) = getSongTitle s

getMPDHandle
  :: String -- ^The host to connect to
 -> String  -- ^The port to connect to
 -> IO (Either String MPDHandle)
getMPDHandle h p = getHandle <$> getMPDSocket h p
  where getHandle (Right x) = (Right (MPDHandle x))
        getHandle (Left x) = (Left x)

-- |Same as 'getMPDHandle' but throws an error instead of returning the error as 'String'
getMPDHandle'
  :: String
  -> String
  -> IO MPDHandle
getMPDHandle' h p = getHandle <$> getMPDHandle h p
  where getHandle (Right x) = x
        getHandle (Left x) = error x
