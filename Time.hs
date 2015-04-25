module Time
where

import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale

getTime :: String -> IO String
getTime str = do
  t <- getCurrentTime
  z <- getCurrentTimeZone
  return $formatTime defaultTimeLocale str $utcToLocalTime z t

getHM :: IO (Int, Int)
getHM = do
  t <- getCurrentTime
  z <- getCurrentTimeZone
  let (LocalTime _ (TimeOfDay h m _)) = utcToLocalTime z t
  return (h, m)
