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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module : Monky.Examples.Combine
Description: Combine multiple modules into one (to avoid seperators)
Maintainer: ongy
Stability: testing
Portability: linux

The functions in this module can be used to avoid printing seperators between modules.

Nearly equivalend example (image is missing):

@
pollPack 1 $ getCPUHandle' ScalingCur
pollPack 1 $ (getFreqHandle ScalingCur) `combine` getRawCPU `combine` getTempHandle'
@

The main function exported by this module are:
  * 'combine'
  * 'combineD'
  * 'combineE'
  * 'combineF'

They are mainly overloads of the same concept, for combinations of 'EvtModule' and 'PollModule'

The mixed functions ('combineD' and 'combineF') create a 'PollModule'. The 'EvtModule' will update
a cache in the new module and output will be updated when the 'PollModule' is asked to do so.

NOTE: because of this, the event will be handled when it is detected, but the output will not update
until the 'PollModule' wrapper ticks once.
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

-- |Wrapper type for 'PollModule's
data (PollModule a, PollModule b) => CombiHandle a b = C a b

instance (PollModule a, PollModule b) => PollModule (CombiHandle a b) where
  getOutput (C a b) = do
    c <- getOutput a
    d <- getOutput b
    return (c ++ d)
  initialize (C a b) = initialize a >> initialize b

-- |Combine two 'PollModule's. The first arguments output will be printed first
combine :: (PollModule a, PollModule b) => IO a -> IO b -> IO (CombiHandle a b)
combine a b = do
  c <- a
  d <- b
  return $ C c d

-- Module handle a, b. IORef for caching a, b. MVar to implement boolean, containing value doesn't matter
-- |Wrapper type for 'EvtModule's
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

-- |Combine two 'EvtModule's. The first argument will be printed first.
combineE :: (EvtModule a, EvtModule b) => IO a -> IO b -> IO (EvtCombi a b)
combineE a b = do
  m1 <- a
  m2 <- b
  r1 <- newIORef []
  r2 <- newIORef []
  m <- newMVar True
  return $ EC m1 m2 r1 r2 m

-- |Wrapper type for mixed combination.
data (EvtModule a, PollModule b) => EPCombi a b = EP a b (IORef [MonkyOut])

instance (EvtModule a, PollModule b) => PollModule (EPCombi a b) where
  initialize (EP a b r) = do
    startEvtLoop a (atomicWriteIORef r)
    initialize b
  getOutput (EP _ b r) = do
    o1 <- readIORef r
    o2 <- getOutput b
    return (o1 ++ o2)

-- |Combine a 'EvtModule' with a 'PollModule'. This will alwasy create a 'PollModule'.
-- |Event updates will not be displayed until the poll modules ticks once.
combineF :: (EvtModule a, PollModule b) => IO a -> IO b -> IO (EPCombi a b)
combineF a b = do
  m1 <- a
  m2 <- b
  r <- newIORef []
  return $ EP m1 m2 r


-- |Wrapper type for mixed combination.
data (PollModule a, EvtModule b) => PECombi a b = PE a b (IORef [MonkyOut])

instance (PollModule a, EvtModule b) => PollModule (PECombi a b) where
  initialize (PE a b r) = do
    startEvtLoop b (atomicWriteIORef r)
    initialize a
  getOutput (PE a _ r) = do
    o1 <- getOutput a
    o2 <- readIORef r
    return (o1 ++ o2)

-- |Look at 'combineF' for documentation
combineD :: (PollModule a, EvtModule b) => IO a -> IO b -> IO (PECombi a b)
combineD a b = do
  m1 <- a
  m2 <- b
  r <- newIORef []
  return $ PE m1 m2 r
