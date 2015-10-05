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
(startLoop, getVersion)
where

import Monky.Event
import Monky.Modules
import Data.IORef (IORef, readIORef, writeIORef, newIORef)

import Control.Applicative((<$>))
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)
import System.Posix.Types (Fd)
import System.Posix.User (getEffectiveUserName)
import Text.Printf (printf)



-- |Export the version of monky to modules
getVersion :: (Int, Int, Int, Int)
getVersion = (1, 2, 0, 4)

-- |The module wrapper used to buffer output strings
data ModuleWrapper = MWrapper Modules (IORef String)

{- Wrapper logic -}
-- |Get the text from a module wrapper
--
-- This function updates the string buffer if needed and reeturns its contents
getWrapperText :: Int -> String -> ModuleWrapper -> IO String
getWrapperText tick u (MWrapper (MW m i) r)
  | i <= 0 = readIORef r
  | i > 0 =
  if tick `mod` i == 0
    then do
      s <- getText u m
      writeIORef r s
      return s
    else readIORef r
getWrapperText _ _ _ = return "Something borked"

-- |print out one line
printMonkyLine :: Int -> String -> [ModuleWrapper] -> IO ()
printMonkyLine _ _ [] = putStrLn "Is this even possible?"
printMonkyLine i u [x] = do
  t <- getWrapperText i u x
  putStrLn t
printMonkyLine i u (x:xs) = do
  t <- getWrapperText i u x
  putStr $printf "%s | " t
  printMonkyLine i u xs


{- Polling logic -}
-- |Update the IORef buffereing the modules section
updateText :: ModuleWrapper -> String -> IO ()
updateText (MWrapper (MW m _) r) u = do
  s <- getText u m
  writeIORef r s


-- |The main loop which waits for events and updates the wrappers
mainLoop :: Int -> String -> [(ModuleWrapper, [Fd])] -> [ModuleWrapper] -> IO()
mainLoop i u f m = do
  printMonkyLine i u m
  hFlush stdout
  threadDelay 1000000
  mainLoop (i+1) u f m


-- |Packs a module into a wrapper with an IORef for cached output
packMod :: Modules -> IO ModuleWrapper
packMod x = do
  ref <- newIORef ("" :: String)
  return (MWrapper x ref)

{- |Starts the main loop for monky

This is the entry-point for printing normal lines.
This function never returns.
-}
startLoop :: [IO Modules] -> IO ()
startLoop mods = do
  u <- getEffectiveUserName
  m <- sequence mods
  l <- mapM packMod m
  f <- rmEmpty <$> mapM getFDList l
  mapM_ (\(mw, _) -> updateText mw u) f
  startEventLoop (map (\(x,y) -> (updateText x u, y)) f)
  mainLoop 0 u f l
  where
    getFDList (MWrapper (MW mw i) ref) = if i <= 0
               then do
                 fds <- getFDs mw
                 return (MWrapper (MW mw i) ref, fds)
               else return (MWrapper (MW mw i) ref, [])
    rmEmpty = filter (\(_, xs) -> xs /= [])

