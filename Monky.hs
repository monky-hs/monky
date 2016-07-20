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
  )
where


import Control.Concurrent (threadDelay)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Monky.Modules
import Control.Monad (when)
import Control.Concurrent (forkIO)


data ModuleWrapper = MWrapper Modules (IORef [MonkyOut])

-- |Packs a module into a wrapper with an IORef for cached output
packMod :: Modules -> IO ModuleWrapper
packMod x@(Poll (NMW m _)) = do
  sref <- newIORef []
  initialize m
  return (MWrapper x sref)
packMod x@(Evt (DW m)) = do
  sref <- newIORef []
  _ <- forkIO (startEvtLoop m sref)
  return $ MWrapper x sref

getWrapperText :: Int -> ModuleWrapper -> IO [MonkyOut]
getWrapperText tick (MWrapper (Poll (NMW m i)) r) = do
  when (tick `mod` i == 0) $ do
    o <- getOutput m
    writeIORef r o
  readIORef r
getWrapperText _ (MWrapper (Evt _) r) = readIORef r

doMonkyLine :: MonkyOutput o => Int -> o -> [ModuleWrapper] -> IO ()
doMonkyLine t o xs =
  doLine o =<< mapM (getWrapperText t) xs

mainLoop :: MonkyOutput o => Int -> o -> [ModuleWrapper] -> IO ()
mainLoop t o xs = do
  doMonkyLine t o xs
  threadDelay 1000000
  mainLoop (t+1) o xs

-- |Start the mainLoop of monky. This should be the return type of your main
startLoop :: MonkyOutput o => IO o -> [IO Modules] -> IO ()
startLoop o mods = do
  m <- sequence mods
  l <- mapM packMod m
  out <- o
  mainLoop 0 out l
