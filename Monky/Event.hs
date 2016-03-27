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
Stability   : internal
Portability : Linux

This module provides a simple api to wait for one of many events in a loop
-}
module Monky.Event
  ( startEventLoop
  )
where


import System.Timeout (timeout)
import Data.Time.Clock.POSIX
import System.Posix.Types (Fd)
import Control.Applicative ((<|>))
import Control.Monad (void, unless)

import Control.Concurrent
import Control.Concurrent.STM.TVar

import Monky.Modules
import Control.Monad.STM


-- |The event wrapper that carries all we need
type MonkyEvent = (Int, Fd -> IO Bool, Modules)
-- |The STM we wait on for the main eventing, has all we need for updating
type EvtSTM = STM (MonkyEvent, Fd, IO ())

-- The return value will be the MonkyEvent (so we can recreate it if we need to)
-- and the IO action to rearm the specific event
data LoopEvt = LoopEvt Int EvtSTM

instance Eq LoopEvt where
  (==) (LoopEvt i _) (LoopEvt j _) = i == j


-- |Grab the event out of loop event
getEvt :: LoopEvt -> EvtSTM
getEvt (LoopEvt _ x) = x

-- |Rearm the event so we can wait on it
armEvent :: TVar Bool -> Fd -> IO ()
armEvent m fd = do
    atomically $writeTVar m False
    void $forkIO (threadWaitRead fd >> atomically (writeTVar m True))

-- |Our own version of threadWaitRead we need for rearm
threadWaitReadSTME :: Fd -> IO (STM(), Fd, IO ())
threadWaitReadSTME fd = do
  m <- newTVarIO False
  let rearm = armEvent m fd
  let waitAction = flip unless retry =<< readTVar m
  rearm -- start it once
  return (waitAction, fd, rearm)


-- |Build a STM event around a MonkyEvent
getSTMEventE :: MonkyEvent -> IO (Maybe EvtSTM)
getSTMEventE evt@(_, _, MW m _) = do
  events <- mapM threadWaitReadSTME =<< getFDs m
  if null events
    then return Nothing
    else return . Just . foldr1 (<|>) . map createEvent $events
  where
    createEvent :: (STM (), Fd, IO ()) -> STM (MonkyEvent, Fd, IO ())
    createEvent (event, fd, rearm) = do
      event
      return (evt, fd, rearm)

-- |Build Events for main loop, Second value is failed activations
getSTMEventEs :: [MonkyEvent] -> IO ([LoopEvt], [MonkyEvent])
getSTMEventEs [] = return ([], [])
getSTMEventEs (x@(i, _, _):xs) = do
  (ys,zs) <- getSTMEventEs xs
  current <- getSTMEventE x
  case current of
    Just y -> return (LoopEvt i y:ys, zs)
    Nothing -> return (ys, x:zs)


-- |Recover events after they failed
recover :: [MonkyEvent] -> IO ([LoopEvt], [MonkyEvent])
recover [] = return ([], [])
recover (x@(i, _, MW m _):xs) = do
  (ys, zs) <- recover xs
  rec <- recoverModule m
  if rec
    then do
      (Just evt) <- getSTMEventE x
      return (LoopEvt i evt:ys,zs)
    else return (ys, x:zs)


stmLoopE :: [LoopEvt] -> [MonkyEvent] -> POSIXTime -> IO ()
-- This should not happen
stmLoopE [] [] _ = return ()
-- If we have no event we could wait on, why wait?
-- Plus the main implementation could not handle an empty list
stmLoopE [] ys _ = do
  threadDelay (10 * 1000 * 1000)
  (xs, zs) <- recover ys
  t <- getPOSIXTime
  stmLoopE xs zs  t
-- The main body of the event list
stmLoopE xs ys t = do
  t' <- getPOSIXTime
  if (t' - t) > 5
    then do
      (rs, ys') <- recover ys
      stmLoopE (rs ++ xs) ys' t'
    else do
    -- This timeout is significantly higher than the check above
    -- The event system should not wake up to often
    -- The only reason this exists is, that we want to be able to recover
    -- modules even if no other has any action
      ret <- timeout (30 * 1000 * 1000) $atomically . foldr1 (<|>) . map getEvt $xs
      case ret of
      -- Nothing means we timed out, so we just restart the loop
        Nothing -> stmLoopE xs ys t
      -- Just x means we got an event before we timed out
        Just (evt@(i, act, _), fd, rearm) -> do
          suc <- act fd
          if suc
          -- rearm and continue loop on success
            then rearm >> stmLoopE xs ys t
          -- remove from timeout list and add to recovery list on fail
            else stmLoopE (filter (rm i) xs) (evt:ys) t
  where
    rm i (LoopEvt j _) = i /= j


startLoopE :: [MonkyEvent] -> IO ()
startLoopE xs = do
  events <- getSTMEventEs xs
  uncurry stmLoopE events 0


createMonkyEvents :: [(Fd -> IO Bool, Modules)] -> [MonkyEvent]
createMonkyEvents = createMonkyEvents' 0
  where
    createMonkyEvents' :: Int -> [(Fd -> IO Bool, Modules)] -> [MonkyEvent]
    createMonkyEvents' _ [] = []
    createMonkyEvents' i ((x, y):xs) = (i, x, y) : createMonkyEvents' (i + 1) xs



{- | Start the event loop

This starts an event loop around the functions given in the list.
Every time one of the fds becomes readable, the associated function will be
called with that fd as argument.

ALL entries in the list have to export some fd(s) ofer getFDs!

If getFDs returns [] the module is assumed to be in a broken state

If this is called with an empty list, nothing will be done.
-}
startEventLoop :: [(Fd -> IO Bool, Modules)] -> IO ()
startEventLoop [] = return ()
startEventLoop xs = void . forkIO . startLoopE $createMonkyEvents xs
