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
{-|
Module      : Monky
Description : The main module for monky
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

This module contains the main logic for monky.
This has to be included in the Monky.hs. The entry point for monky is startLoop
which has to be called with a ['IO' 'Modules'].
This type was chosen to make the syntax in the config file prettier.

The submodules of this generally provide an interface based on handles.
To use them, get the handle at the beginning of you application and hand it to
other functions later on.
-}
module Monky
  ( startLoop
  , startLoopT
  )
where

import System.Timeout
import Data.Time.Clock.POSIX
import Control.Concurrent.MVar
import Data.IORef (IORef, readIORef, writeIORef, newIORef, atomicWriteIORef)
import Monky.Modules
import Control.Monad (when)
import Control.Concurrent (forkIO)


data ModuleWrapper = MWrapper Modules (IORef [MonkyOut])

-- |Packs a module into a wrapper with an IORef for cached output
packMod :: MVar Bool -> Modules -> IO ModuleWrapper
packMod _ x@(Poll (NMW m _)) = do
    sref <- newIORef []
    initialize m
    return (MWrapper x sref)
packMod mvar x@(Evt (DW m)) = do
    sref <- newIORef []
    _ <- forkIO . startEvtLoop m $ \val -> do
        atomicWriteIORef sref val
        putMVar mvar True
    return $ MWrapper x sref

getWrapperText :: Int -> ModuleWrapper -> IO [MonkyOut]
getWrapperText tick (MWrapper (Poll (NMW m i)) r) = do
  when (tick `mod` i == 0) (writeIORef r =<< getOutput m)
  readIORef r
getWrapperText _ (MWrapper (Evt _) r) = readIORef r

doMonkyLine :: MonkyOutput o => Int -> o -> [ModuleWrapper] -> IO ()
doMonkyLine t o xs =
  doLine o =<< mapM (getWrapperText t) xs

doCachedLine :: MonkyOutput o => o -> [ModuleWrapper] -> IO ()
doCachedLine out xs =
    doLine out =<< mapM (\(MWrapper _ r) -> readIORef r) xs

waitTick :: MonkyOutput o => Int -> MVar Bool -> o -> [ModuleWrapper] -> IO ()
waitTick limit mvar out xs = do
    pre <- getPOSIXTime
    ret <- timeout limit $ takeMVar mvar
    case ret of
        Nothing -> return ()
        Just _ -> do
            doCachedLine out xs
            post <- getPOSIXTime
            let passed = round . (* 1000000) $ post - pre
            if passed > limit
                then return ()
                else waitTick (limit - passed) mvar out xs

mainLoop :: MonkyOutput o => Int -> MVar Bool -> Int -> o -> [ModuleWrapper] -> IO ()
mainLoop l r t o xs = do
  doMonkyLine t o xs
  waitTick l r o xs
  mainLoop l r (t+1) o xs

{- | Start the mainLoop of monky. With custom tick timer (in μs)

For 1s tick-timer pass @1000000@ to this.
This does not scale the timers for collection modules passed into it.
So if @500000@ is passed here, and a collector should update every second
it needs to update every 2nd tick, so it has to be scaled.

@
    startLoop getAsciiOut [ pollPack 1 $ getRawCPU ]

    startLoopT 500000 getAsciiOut [ pollPack 2 $ getRawCPU ]
@
-}
startLoopT :: MonkyOutput o => Int -> IO o -> [IO Modules] -> IO ()
startLoopT t out mods = do
    mvar <- newEmptyMVar
    m <- sequence mods
    l <- mapM (packMod mvar) m
    o <- out
    mainLoop t mvar 0 o l

-- | Start the mainLoop of monky. This should be the return type of your main
startLoop :: MonkyOutput o => IO o -> [IO Modules] -> IO ()
startLoop out mods = startLoopT 1000000 out mods
