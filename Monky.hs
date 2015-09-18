module Monky
(startLoop)
where

import Modules
import Data.IORef (IORef, readIORef, writeIORef, newIORef)

import Control.Applicative((<$>))
import System.IO (hFlush, stdout)
import System.Posix.IO.Select (select')
import System.Posix.IO.Select.Types (Timeout(..), CTimeval(..))
import System.Posix.Types (Fd)
import System.Posix.User (getEffectiveUserName)
import Text.Printf (printf)


-- |The module wrapper used to buffer output strings
data ModuleWrapper = MWrapper Modules (IORef String)

{- Wrapper logic -}
-- |Get the text from a module wrapper
--
-- This function updates the string buffer if needed and reeturns its contents
getWrapperText :: Int -> String -> ModuleWrapper -> IO String
getWrapperText i u (MWrapper (MW m) r)
  | getInterval m <= 0 = readIORef r
  | getInterval m > 0 =
  if i `mod` getInterval m == 0
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
  printf "%s\n" t
printMonkyLine i u (x:xs) = do
  t <- getWrapperText i u x
  printf "%s | " t
  printMonkyLine i u xs


{- Polling logic -}
-- |Update the IORef buffereing the modules section
updateText :: ModuleWrapper -> String -> IO ()
updateText (MWrapper (MW m) r) u = do
  s <- getText u m
  writeIORef r s

-- |Update a single wrapper based on the file descriptor found in select output
doUpdate :: Fd -> [(ModuleWrapper, [Fd])] -> String -> IO ()
doUpdate _ [] _ = return ()
doUpdate f ((mw, fds):xs) u = if f `elem` fds
  then updateText mw u
  else doUpdate f xs u

-- |Internal function for doUpdates
doUpdatesInt :: [Fd] -> [(ModuleWrapper, [Fd])] -> String -> IO ()
doUpdatesInt [] _ _ = return ()
doUpdatesInt (fd:fds) xs u = do
  doUpdate fd xs u
  doUpdatesInt fds xs u

-- |Update the modules that triggered the select call
doUpdates :: Maybe ([Fd], [Fd], [Fd]) -> [(ModuleWrapper, [Fd])] -> String -> IO ()
doUpdates (Just (fds, _, _)) xs u = doUpdatesInt fds xs u
doUpdates Nothing _ _ = return ()


{- Main loop -}

-- |The main loop which waits for events and updates the wrappers
mainLoop :: Int -> String -> [(ModuleWrapper, [Fd])] -> [ModuleWrapper] -> IO()
mainLoop i u f m = do
  printMonkyLine i u m
  hFlush stdout
  e <- select' (concatMap snd f) [] [] (Time (CTimeval 1 0))
  doUpdates e f u
  mainLoop (i+1) u f m


-- |Packs a module into a wrapper with an IORef for cached output
packMod :: Modules -> IO ModuleWrapper
packMod x = do
  ref <- newIORef ("" :: String)
  return (MWrapper x ref)

-- |Creates the IORefs for the wrappers and sets up polling fds
startLoop :: [IO Modules] -> IO ()
startLoop mods = do
  u <- getEffectiveUserName
  m <- sequence mods
  l <- mapM packMod m
  f <- rmEmpty <$> mapM getFDList l
  mapM_ (\(mw, _) -> updateText mw u) f
  mainLoop 0 u f l
  where
    getFDList (MWrapper (MW mw) ref) = if getInterval mw <= 0
               then do
                 fds <- getFDs mw
                 return (MWrapper (MW mw) ref, fds)
               else return (MWrapper (MW mw) ref, [])
    rmEmpty = filter (\(_, xs) -> xs /= [])

