{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module : Monky.Examples.Combine
Description: Combine multiple modules into one (to avoid seperators)
Maintainer: ongy
Stability: testing
Portability: linux


Nearly equivalend example (image is missing):

@
pollPack 1 $ getCPUHandle' ScalingCur
pollPack 1 $ (getFreqHandle ScalingCur) `combine` getRawCPU `combine` getTempHandle'
@

-}
module Monky.Examples.Combine
  ( CombiHandle
  , combine

  , EvtCombi
  , combineE

  , EPCombi
  , combineF

  , PECombi
  , combineD
  )
where

import Monky.Modules
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, withMVar, newMVar)
import Data.IORef (IORef, readIORef, atomicWriteIORef, newIORef)


data (PollModule a, PollModule b) => CombiHandle a b = C a b

instance (PollModule a, PollModule b) => PollModule (CombiHandle a b) where
  getOutput (C a b) = do
    c <- getOutput a
    d <- getOutput b
    return (c ++ d)
  initialize (C a b) = initialize a >> initialize b

combine :: (PollModule a, PollModule b) => IO a -> IO b -> IO (CombiHandle a b)
combine a b = do
  c <- a
  d <- b
  return $ C c d

-- Module handle a, b. IORef for caching a, b. MVar to implement boolean, containing value doesn't matter
data (EvtModule a, EvtModule b) => EvtCombi a b = EC a b (IORef [MonkyOut]) (IORef [MonkyOut]) (MVar Bool)

instance (EvtModule a, EvtModule b) => EvtModule (EvtCombi a b) where
  startEvtLoop (EC a b r1 r2 m) act = do
    _ <- forkIO $ startEvtLoop a (\out -> withMVar m $ \_ -> do
      atomicWriteIORef r1 out
      other <- readIORef r2
      act (out ++ other))
    startEvtLoop b (\out -> withMVar m $ \_ -> do
      atomicWriteIORef r2 out
      other <- readIORef r1
      act (other ++ out))

combineE :: (EvtModule a, EvtModule b) => IO a -> IO b -> IO (EvtCombi a b)
combineE a b = do
  m1 <- a
  m2 <- b
  r1 <- newIORef []
  r2 <- newIORef []
  m <- newMVar True
  return $ EC m1 m2 r1 r2 m

data (EvtModule a, PollModule b) => EPCombi a b = EP a b (IORef [MonkyOut])

instance (EvtModule a, PollModule b) => PollModule (EPCombi a b) where
  initialize (EP a b r) = do
    startEvtLoop a (atomicWriteIORef r)
    initialize b
  getOutput (EP _ b r) = do
    o1 <- readIORef r
    o2 <- getOutput b
    return (o1 ++ o2)

combineF :: (EvtModule a, PollModule b) => IO a -> IO b -> IO (EPCombi a b)
combineF a b = do
  m1 <- a
  m2 <- b
  r <- newIORef []
  return $ EP m1 m2 r


data (PollModule a, EvtModule b) => PECombi a b = PE a b (IORef [MonkyOut])

instance (PollModule a, EvtModule b) => PollModule (PECombi a b) where
  initialize (PE a b r) = do
    startEvtLoop b (atomicWriteIORef r)
    initialize a
  getOutput (PE a _ r) = do
    o1 <- getOutput a
    o2 <- readIORef r
    return (o1 ++ o2)


combineD :: (PollModule a, EvtModule b) => IO a -> IO b -> IO (PECombi a b)
combineD a b = do
  m1 <- a
  m2 <- b
  r <- newIORef []
  return $ PE m1 m2 r
