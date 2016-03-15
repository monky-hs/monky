module Monky.NewNetwork
where

import Data.List (isPrefixOf)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import System.Directory (getDirectoryContents)
import Data.Time.Clock.POSIX
import Monky.Utility
import Data.IORef

data NetState = Down | Up

data NetworkHandle = NetH File File File (IORef Int) (IORef Int) (IORef POSIXTime)

type NetHandle = (String, NetworkHandle)

type Handles = IntMap NetHandle

basePath :: String
basePath = "/sys/class/net/"

readPath :: String
readPath = "/statistics/rx_bytes"

writePath :: String
writePath = "/statistics/tx_bytes"

statePath :: String
statePath = "/operstate"


getReadWrite :: NetworkHandle -> IO (Int, Int)
getReadWrite (NetH readf writef _ readr writer timer) = do
  nread <- readValue readf
  nwrite <- readValue writef
  ntime <- getPOSIXTime
  oread <- readIORef readr
  owrite <- readIORef writer
  otime <- readIORef timer
  let cread = oread - nread
  let cwrite = owrite - nwrite
  let ctime = otime - ntime
  writeIORef readr nread
  writeIORef writer nwrite
  writeIORef timer ntime
  return ((cread  * 8) `sdiv` round ctime,
          (cwrite * 8) `sdiv` round ctime)
  where sdiv x 0 = x
        sdiv x y = x `div` y


getState :: NetworkHandle -> IO NetState
getState (NetH _ _ statef _ _ _) = do
  state <- readLine statef
  if state == "down"
    then return Down
    else if state == "up"
      then return Up
      else error ("Don't know the network state \"" ++ state ++ "\" yet")


getReadWriteM :: NetworkHandle -> IO (Maybe (Int, Int))
getReadWriteM h = do
  state <- getState h
  case state of
    Up -> Just <$> getReadWrite h
    Down -> return Nothing


foldF :: NetworkHandle -> IO (Maybe (Int, Int)) -> IO (Maybe (Int, Int))
foldF h o = do
  m <- getReadWriteM h
  case m of
    (Just (r, w)) -> do
      om <- o
      case om of
        Just (oldr, oldw) -> return $ Just (oldr + r, oldw + w)
        Nothing -> return $ Just (r, w)
    Nothing -> o


getMultiReadWrite :: Handles -> IO (Maybe (Int, Int))
getMultiReadWrite =
  foldr (\(_, v) -> foldF v) (return start)
  where start = Just (0, 0)


getNetworkHandle :: String -> IO NetworkHandle
getNetworkHandle dev = do
  let path = basePath ++ dev
  readf <- fopen $ path ++ readPath
  writef <- fopen $ path ++ writePath
  statef <- fopen $ path ++ statePath
  readr <- newIORef (1 :: Int)
  writer <- newIORef (1 :: Int)
  timer <- newIORef (0 :: POSIXTime)
  return $NetH readf writef statef readr writer timer

-- TODO check this
closeNetworkHandle :: NetworkHandle -> IO ()
closeNetworkHandle (NetH readf writef statef _ _ _) =
  fclose readf >> fclose writef >> fclose statef


gotNew :: Int -> String -> Handles -> IO Handles
gotNew index name m = do
  case IM.lookup index m of
    Nothing -> do
      h <- getNetworkHandle name
      return $IM.insert index (name, h) m
    Just (x, v) -> if x == name
      then return m
      else do
        h <- getNetworkHandle name
        closeNetworkHandle v
        return $IM.adjust (\_ -> (name, h)) index m


lostOld :: Int -> Handles -> IO Handles
lostOld index m = case IM.lookup index m of
  Nothing -> return m
  (Just (_, h)) ->
    closeNetworkHandle h >>
    return (IM.delete index m) -- TOOD close handle

buildMap :: [NetHandle] -> Int -> Handles -> Handles
buildMap [] _ m = m
buildMap (x:xs) i m = buildMap xs (i + 1) (IM.insert i x m)

getNetworkHandles :: (String -> Bool) -> IO Handles
getNetworkHandles f = do
  interfaces <- filter (\s -> f s && noHidden s) <$> getDirectoryContents basePath
  handles <- mapM getH interfaces
  return $buildMap handles 0 IM.empty
  where getH dev = do
          h <- getNetworkHandle dev
          return (dev, h)
        -- Hiddens will be the . and .., nothing else
        noHidden = not . isPrefixOf "."
