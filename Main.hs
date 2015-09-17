import Modules

import Data.IORef

import Control.Monad (liftM)
import System.IO
import System.Posix.Types
import System.Posix.User (getEffectiveUserName)
import System.Posix.IO.Select
import System.Posix.IO.Select.Types
import Text.Printf (printf)


data ModuleWrapper = MWrapper Modules (IORef String)

{- Wrapper logic -}
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
updateText :: ModuleWrapper -> String -> IO ()
updateText (MWrapper (MW m) r) u = do
  s <- getText u m
  writeIORef r s

doUpdate :: Fd -> [(ModuleWrapper, [Fd])] -> String -> IO ()
doUpdate _ [] _ = return ()
doUpdate f ((mw, fds):xs) u = if f `elem` fds
  then updateText mw u
  else doUpdate f xs u

doUpdatesInt :: [Fd] -> [(ModuleWrapper, [Fd])] -> String -> IO ()
doUpdatesInt [] _ _ = return ()
doUpdatesInt (fd:fds) xs u = do
  doUpdate fd xs u
  doUpdatesInt fds xs u

doUpdates :: Maybe ([Fd], [Fd], [Fd]) -> [(ModuleWrapper, [Fd])] -> String -> IO ()
doUpdates (Just (fds, _, _)) xs u = doUpdatesInt fds xs u
doUpdates Nothing _ _ = return ()


{- Main loop -}

mainLoop :: Int -> String -> [(ModuleWrapper, [Fd])] -> [ModuleWrapper] -> IO()
mainLoop i u f m = do
  printMonkyLine i u m
  hFlush stdout
  e <- select' (concatMap snd f) [] [] (Time (CTimeval 1 0))
  doUpdates e f u
  mainLoop (i+1) u f m



packMod :: Modules -> IO ModuleWrapper
packMod x = do
  ref <- newIORef ("" :: String)
  return (MWrapper x ref)


startLoop :: String -> IO [Modules] -> IO ()
startLoop u m = do
  n <- liftM (map packMod) m
  l <- sequence n
  f <- liftM rmEmpty $sequence $getFDList l
  sequence_ $map (\(mw, _) -> updateText mw u) f
  mainLoop 0 u f l
  where
    getFDList = map (\(MWrapper (MW mw) ref) -> if getInterval mw <= 0
               then do 
                 fds <- getFDs mw
                 return (MWrapper (MW mw) ref, fds)
               else return (MWrapper (MW mw) ref, []))
    rmEmpty = filter (\(_, xs) -> xs /= [])



main :: IO()
main = do
  user <- getEffectiveUserName
  startLoop user getModules
