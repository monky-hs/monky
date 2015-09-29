{-# LANGUAGE StandaloneDeriving #-}
module Monky.Examples.MPD
(MPDHandle, getMPDHandle)
where


import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Monky.Modules
import qualified Data.Map as M
import qualified Network.MPD as MPD


deriving instance Show MPD.ACKType

getPlayingSong :: MPD.State -> MPD.MPD (Maybe MPD.Song)
getPlayingSong MPD.Playing = MPD.currentSong
getPlayingSong _ = return Nothing


extractTitle :: MPD.Song -> Maybe String
extractTitle (MPD.Song _ tags _ _ _ _) = 
  fmap (MPD.toString . head) $M.lookup MPD.Title tags


getSong :: MPD.MPD (Maybe MPD.Song)
getSong = do
  stat <- MPD.status
  getPlayingSong (MPD.stState stat)


getRes :: Either MPD.MPDError a -> (a -> String) -> String
getRes (Left  MPD.NoMPD) _= "MPD not responding"
getRes (Left (MPD.ConnectionError _)) _ = "Connection Error"
getRes (Left (MPD.Unexpected x)) _ = x
getRes (Left (MPD.Custom x)) _ = x
getRes (Left (MPD.ACK y x)) _ = show y ++ ": " ++ x
getRes (Right x) f = f x


getCurrentPlaying :: MPDHandle -> IO String
getCurrentPlaying (MPDHandle h p) = do
  song <- MPD.withMPD_ (Just h) (Just p) getSong
  let title = getRes song (fromMaybe "Not Playing" . join . fmap extractTitle)
  return title


data MPDHandle = MPDHandle String String

instance Module MPDHandle where
  getText _ = getCurrentPlaying

getMPDHandle
  :: String -- ^The host to connect to
 -> String  -- ^The port to connect to
 -> IO MPDHandle
getMPDHandle h p = return $MPDHandle h p
