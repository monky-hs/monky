module Time
where

import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale

data TimeHandle = TimeH String

getFormat :: TimeHandle -> String
getFormat (TimeH f) = f

getTime :: String -> IO String
getTime str = do
  t <- getCurrentTime
  z <- getCurrentTimeZone
  return $formatTime Data.Time.Format.defaultTimeLocale str $utcToLocalTime z t

getHM :: IO (Int, Int)
getHM = do
  t <- getCurrentTime
  z <- getCurrentTimeZone
  let (LocalTime _ (TimeOfDay h m _)) = utcToLocalTime z t
  return (h, m)

getTimeHandle :: String -> TimeHandle
getTimeHandle format = (TimeH format)
