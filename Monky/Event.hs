{-
    Copyright 2015 Markus Ongyerth, Stephan Guenther

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
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.Event
Description : Abstraction over event system for monky
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides a simple api to wait for one of many events in a loop
-}
module Monky.Event
(startEventLoop)
where


import System.Posix.Types (Fd)
import Control.Applicative ((<|>))
import Control.Monad (void)

import Control.Concurrent
import Control.Concurrent.STM.TVar

import Control.Monad.STM


armEvent :: TVar Bool -> Fd -> IO ()
armEvent m fd = do
    atomically $writeTVar m False
    void $forkIO (threadWaitRead fd >> atomically (writeTVar m True))

-- variation to threadReadWaitSTM that allows rearming of the STM
threadWaitReadSTM' :: Fd -> IO (STM(), IO ())
threadWaitReadSTM' fd = do
  m <- newTVarIO False
  let rearm = armEvent m fd
  let waitAction = do b <- readTVar m
                      if b then return () else retry
  rearm -- start it once
  return (waitAction, rearm)



getSTMEvent :: (Fd -> IO ()) -> Fd -> IO (STM (IO ()))
getSTMEvent act fd = do
    (event,rearm) <- threadWaitReadSTM' fd
    return (event >> return (act fd >> rearm))


getSTMEvents :: [(Fd -> IO (), [Fd])] -> IO [STM (IO ())]
getSTMEvents [] = return []
getSTMEvents ((act, fds):xs) = do
  events <- mapM (getSTMEvent act) fds
  others <- getSTMEvents xs
  return (events ++ others)


stmLoop :: STM (IO ()) -> IO ()
stmLoop events = do
  act <- atomically events
  act
  stmLoop events


startSTMEvents :: [(Fd -> IO (), [Fd])] -> IO ()
startSTMEvents xs = do
  events <- getSTMEvents xs
  if null events
    then return ()
    else stmLoop $foldr1 ((<|>)) events

{- | Start the event loop

This starts an event loop around the functions given in the list.
Every time one of the fds becomes readable, the associated function will be
called with that fd as argument.
-}
startEventLoop :: [(Fd -> IO (), [Fd])] -> IO ()
startEventLoop xs = forkIO (startSTMEvents xs) >> return ()
