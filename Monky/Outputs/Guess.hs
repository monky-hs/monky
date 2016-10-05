{-
    Copyright 2016 Markus Ongyerth

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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.Outputs.Guess
Description : Guess the output that should be used based on pipe
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Outputs.Guess
  ( guessOutput
  , GuessOut
  )
where

import Data.Text (Text)

import Control.Exception (try)
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import System.Directory (getDirectoryContents)
import System.IO (hIsTerminalDevice, stdout)
import System.Posix.Files (readSymbolicLink)

import Monky.Modules
import Monky.Outputs.Fallback (chooseTerminalOut)
import Monky.Outputs.Show (getShowOut)
import Monky.Outputs.Dzen2 (getDzenOut)
import Monky.Outputs.I3 (getI3Output)
import Monky.Outputs.Serialize (getSerializeOut)

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

data Output
  = Terminal
  | Process String
  | Other
  deriving (Eq, Ord, Show)

-- | Type wrapper for this to work
data GuessOut = forall a . MonkyOutput a => GO a

instance MonkyOutput GuessOut where
  doLine (GO o) = doLine o


networkOuts :: [String]
networkOuts = ["netcat", "nc", "socat"]


fdToPath :: String -> Int -> IO String
fdToPath proc fd = readSymbolicLink ("/proc/" ++ proc ++ "/fd/" ++ show fd)


getProcFds :: String -> IO (Maybe (String, [(Int, String)]))
getProcFds proc = do
  ret <- try $ do
    fds <- map read . filter (not . isPrefixOf ".") <$> getDirectoryContents ("/proc/" ++ proc ++ "/fd/")
    paths <- mapM (fdToPath proc) fds
    exe <- readSymbolicLink ("/proc/" ++ proc ++ "/exe")
    return (reverse . takeWhile (/= '/') . reverse $ exe, zip fds paths)
  return $ case ret of
    (Left (_ :: IOError))  -> Nothing
    (Right x) -> Just x


hasPipe :: String -> (String, [(Int, String)]) -> Bool
hasPipe pipe (_, xs) = any (\(fd, path) -> path == pipe && fd /= 1) xs


pickCandidate :: String -> [(String, [(Int, String)])] -> Maybe String
pickCandidate _ [] = Nothing
pickCandidate _ [(x, _)] = Just x
pickCandidate pipe xs =
  let std = filter (\(_, ys) -> any (\(i, path) -> i == 1 && path == pipe) ys) xs in
    Just . fst $ if null std
      then head xs
      else head std


getOutputType :: IO Output
getOutputType = do
  term <- hIsTerminalDevice stdout
  if term
    then return Terminal
    else do
      procs <- filter (all isDigit) <$> getDirectoryContents "/proc"
      fds <- catMaybes <$> mapM getProcFds procs
      own <- readSymbolicLink ("/proc/self/fd/1")

      let candidates = filter (hasPipe own) fds
      let candidate = pickCandidate own candidates
      return $ case candidate of
        Nothing -> Other
        (Just x) -> Process x

-- Same inputs as 'gessOutput'
chooseProcessOut
  :: Int
  -> Text
  -> String
  -> IO GuessOut
chooseProcessOut height path x
  | x == "dzen2" = GO <$> getDzenOut height path
  | x == "i3bar" = GO <$> getI3Output
  | x `elem`networkOuts = GO <$> getSerializeOut
  | otherwise = GO <$> getShowOut

-- | Guess output based on isatty and other side of the stdout fd
guessOutput
  :: Int -- ^Dzen height
  -> Text -- ^Dzen xbm path
  -> IO GuessOut
guessOutput height path = do
  out <- getOutputType
  case out of
    Terminal -> GO <$> chooseTerminalOut
    Other -> {- for other we just use show -} GO <$> getShowOut
    (Process x) -> chooseProcessOut height path x
