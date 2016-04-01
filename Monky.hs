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
(startLoop)
where


import Control.Concurrent (threadDelay)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Monky.Event
import Monky.Modules
import Control.Monad (when)
import System.IO (hFlush, stdout)
import System.Posix.Types (Fd)
import System.Posix.User (getEffectiveUserName)


-- |The module wrapper used to buffer output strings
data ModuleWrapper = MWrapper Modules (IORef String) (IORef Bool)

{- Wrapper logic -}
-- |Get the text from a module wrapper
--
-- This function updates the string buffer if needed and reeturns its contents
getWrapperText :: Int -> String -> ModuleWrapper -> IO String
getWrapperText tick u (MWrapper (MW m i) r _)
  | i <= 0 = readIORef r
  | i > 0 = do
    when (tick `mod` i == 0) $do
      s <- getText u m
      writeIORef r s
    readIORef r
getWrapperText _ _ _ = return "Something borked"

-- |print out one line
printMonkyLine :: Int -> String -> [ModuleWrapper] -> IO ()
printMonkyLine _ _ [] = putStrLn "Is this even possible?"
printMonkyLine i u [x] = do
  t <- getWrapperText i u x
  putStrLn t
printMonkyLine i u (x:xs) = do
  t <- getWrapperText i u x
  putStr t
  putStr " | "
  printMonkyLine i u xs


{- Polling logic -}
-- |Update the IORef buffereing the modules section
updateText :: ModuleWrapper -> String -> IO ()
updateText (MWrapper (MW m _) r b) u = do
  rec <- readIORef b
  if rec
    then recoverModule m >>= (\v -> when v (writeIORef b False >> doUpdate))
    else doUpdate
  where
    doUpdate = do
      ret <- getTextFailable u m 
      case ret of
        Just s -> writeIORef r s
        Nothing -> writeIORef b True >> writeIORef r "Broken"

{- |Update the IORef of a modules from an event

This function returns False if the module entered a broken state and should
not be called from the fd events anymore but should be called in a recovery loop
-}
updateText' :: ModuleWrapper -> String -> Fd -> IO Bool
updateText' (MWrapper (MW m _) r _) u fd = do
  ret <- getEventTextFailable fd u m
  case ret of
    Just s -> writeIORef r s >> return True
    Nothing -> writeIORef r "Broken" >> return False


-- |The main loop which waits for events and updates the wrappers
mainLoop :: Int -> String  -> [ModuleWrapper] -> IO()
mainLoop i u m = do
  printMonkyLine i u m
  hFlush stdout
  threadDelay 1000000
  mainLoop (i+1) u m


-- |Packs a module into a wrapper with an IORef for cached output
packMod :: Modules -> IO ModuleWrapper
packMod x = do
  sref <- newIORef ("" :: String)
  bref <- newIORef (True :: Bool)
  return (MWrapper x sref bref)

initModule :: ModuleWrapper -> IO ()
initModule (MWrapper (MW m _) sref bref) = do
  ret <- setupModule m
  if ret
    then return ()
    else do
      writeIORef sref "Init failed"
      writeIORef bref True
      return ()

{- |Starts the main loop for monky

This is the entry-point for printing normal lines.
This function never returns.
-}
startLoop :: [IO Modules] -> IO ()
startLoop mods = do
  u <- getEffectiveUserName
  m <- sequence mods
  l <- mapM packMod m
  mapM_ (flip updateText u) l
  let f = rmEmpty l
  mapM_ initModule f
  startEventLoop (map (\y@(MWrapper x _ _) -> (updateText' y u, x)) f)
  mainLoop 0 u l
  where
    rmEmpty = filter (\(MWrapper (MW _ i) _ _) -> i <= 0)
