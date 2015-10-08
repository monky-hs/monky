{-# LANGUAGE CPP #-}
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


startEventLoop :: [(Fd -> IO (), [Fd])] -> IO ()
startEventLoop xs = forkIO (startSTMEvents xs) >> return ()
