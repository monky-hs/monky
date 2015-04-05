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
