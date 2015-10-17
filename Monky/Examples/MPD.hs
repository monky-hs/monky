{-|
Module      : Monky.Examples.MPD
Description : An example module instance for the MPD module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
module Monky.Examples.MPD
(MPDHandle, getMPDHandle, getMPDHandle')
where


import Data.Maybe (fromMaybe)
import Monky.MPD
import Monky.Modules
import System.Posix.Types (Fd)


#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif


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
data MPDHandle = MPDHandle MPDSocket

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
  getText _ (MPDHandle s) = getSongTitle s
  getEventText _ _ (MPDHandle s) = getEvent s
  getFDs (MPDHandle s) = getFd s

-- |Get the 'MPDHandle' or an error
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
