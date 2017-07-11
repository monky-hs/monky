{-
    Copyright 2017 Donat Khabibullaev

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
{-|
Module      : Monky.Examples.Tminus
Description : Countdown clock for a specific time of day
Stability   : testing

-}
module Monky.Examples.Tminus
  ( getTargetHandle
  )
where

import Control.Applicative (pure)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime
import System.Time
import qualified Data.Text as T

import Monky.Modules

getTargetHandle :: [Int] -> IO TargetTime
getTargetHandle xs = pure $ TargetTime xs

-- target time
-- HH MM SS
newtype TargetTime = TargetTime [Int]

{- | Render a number of seconds as a human-readable amount.  Shows the two
most significant places.  For instance:

>renderSecs 121 = "2m 1s"
-}
renderSecs :: Integer -> String
renderSecs i = renderTD $ diffClockTimes (TOD i 0) (TOD 0 0)

{- | Like 'renderSecs', but takes a TimeDiff instead of an integer second
count. -}
renderTD :: TimeDiff -> String
renderTD itd =
    case workinglist of
      [] -> "0s"
      _ -> unwords . map (\(q, s) -> show q ++ [s]) $ workinglist
    where td = normalizeTimeDiff itd
          suffixlist = "yMdhms"
          quantlist = (\(TimeDiff y mo d h m s _) -> [y, mo, d, h, m, s]) td
          zippedlist = zip quantlist suffixlist
          -- Drop all leading elements that are 0, then take at most 2
          workinglist = take 2 . dropWhile (\(q, _) -> q == 0) $ zippedlist

{- | Convert a list of hour, minute, seconds to seconds. -}
timeToSeconds :: [Int] -> Int
timeToSeconds l = sum (zipWith (*) l [3600, 60, 1])

diffTime :: Int -> Int -> Int
diffTime a b
    | a < b = a + (86400 - b)
    | otherwise = a - b

getCurrent :: TargetTime -> IO [MonkyOut]
getCurrent (TargetTime target) = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
        nowSeconds = timeToSeconds [hour, minute, (floor second)]
        targetSeconds = timeToSeconds target
        delta = diffTime targetSeconds nowSeconds
    pure $ [ MonkyPlain . T.pack $ renderSecs (toInteger delta) ]

instance PollModule TargetTime where
    getOutput t = getCurrent t
