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
{-|
Module      : Monky.Network.Static
Description : Allows access to information about they systems network
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module should be used for static network configurations
-}
module Monky.Network.Static
  ( getReadWrite
  , NetworkHandle
  , getNetworkHandle
  , closeNetworkHandle
  )
where

import Monky.Utility
import Data.Time.Clock.POSIX
import Data.IORef
import System.IO.Error (catchIOError)


-- |Current state of network device
data NetState
  = Down -- ^It is down, consider off
  | Up -- ^It is up, consider on
  | Unknown -- ^Unknown, kernel docu says to consider on
  | Dormant -- ^Dormant, consider off

-- |Get the current network adapter state from kernel
getState :: NetworkHandle -> IO NetState
getState (NetH _ _ statef _ _ _) = do
-- the read can throw an exception if the interface disapperad, we just consider it down
  state <- catchIOError (readLine statef) (\_ -> return "down")
  return $ case state of
    "up" -> Up
    "down" -> Down
    "unknown" -> Unknown
    "dormant" -> Dormant
    "lowerlayerdown" -> Down
    _ -> error ("Don't know the network state \"" ++ state ++ "\" yet")


-- |Internal handle represanting exactly one interface
data NetworkHandle = NetH File File File (IORef Int) (IORef Int) (IORef POSIXTime)

basePath :: String
basePath = "/sys/class/net/"


readPath :: String
readPath = "/statistics/rx_bytes"


writePath :: String
writePath = "/statistics/tx_bytes"


statePath :: String
statePath = "/operstate"

-- Pure part of read/write rate calculation
calculateRates :: Int -> Int -> Int -> Int -> POSIXTime -> POSIXTime -> (Int, Int)
calculateRates oread nread owrite nwrite otime ntime =
  let cread = oread - nread
      cwrite = owrite - nwrite
      ctime = otime - ntime in
    ((cread * 8) `sdivBound` round ctime,
     (cwrite * 8) `sdivBound` round ctime)

-- IO Part of read/write rate calculation
getReadWriteReal :: NetworkHandle -> IO (Int, Int)
getReadWriteReal (NetH readf writef _ readref writeref timeref) = do
  nread <- readValue readf
  nwrite <- readValue writef
  ntime <- getPOSIXTime

  oread <- readIORef readref
  owrite <- readIORef writeref
  otime <- readIORef timeref

  writeIORef readref nread
  writeIORef writeref nwrite
  writeIORef timeref ntime

  return $ calculateRates oread nread owrite nwrite otime ntime

-- |Get the (read, write) rate of the network interface. This is averaged over the time between calls
getReadWrite :: NetworkHandle -> IO (Maybe (Int, Int))
getReadWrite h = do
  state <- getState h
  case state of
    Up -> fmap Just . getReadWriteReal $ h
    Unknown -> fmap Just . getReadWriteReal $ h
    _ -> return Nothing

-- |Get a 'NetworkHandle'
getNetworkHandle
  :: String -- ^Name of the interface to monitor
  -> IO NetworkHandle
getNetworkHandle dev = do
  readf <- fopen $path ++ readPath
  writef <- fopen $path ++ writePath
  statef <- fopen $path ++ statePath
  readref <- newIORef (1 :: Int)
  writeref <- newIORef (1 :: Int)
  timeref <- newIORef (0 :: POSIXTime)
  return $NetH readf writef statef readref writeref timeref
  where path = basePath ++ dev

-- |Close a network handle after it is no longer needed (the device disappeared)
closeNetworkHandle :: NetworkHandle -> IO ()
closeNetworkHandle (NetH readf writef statef _ _ _) =
  mapM_ fclose [readf, writef, statef]
