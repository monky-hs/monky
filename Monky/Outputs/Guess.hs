{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
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
import System.IO.Error (IOError)
import System.Posix.Files (readSymbolicLink)

import Monky.Modules
import Monky.Outputs.Fallback (chooseTerminalOut)
import Monky.Outputs.Show (getShowOut)
import Monky.Outputs.Dzen2 (getDzenOut)
import Monky.Outputs.Serialize (getSerializeOut)

data Output
  = Terminal
  | Process String
  | Other
  deriving (Eq, Ord, Show)

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
  | x `elem`networkOuts = GO <$> getSerializeOut
  | otherwise = GO <$> getShowOut

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
