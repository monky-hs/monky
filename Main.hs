import Modules

import Data.IORef

import Config
import Control.Concurrent (threadDelay)
import Control.Monad (liftM)
import System.IO
import System.Posix.User (getEffectiveUserName)
import Text.Printf (printf)
import qualified Data.Text


data ModuleWrapper = MWrapper Modules (IORef String)


getWrapperText :: Int -> String -> ModuleWrapper -> IO String
getWrapperText i u (MWrapper (MW m) r) = do
  if i `mod` (getInterval m) == 0
    then do
      s <- getText u m
      writeIORef r s
      return s
    else readIORef r


printMonkyLine :: Int -> String -> [ModuleWrapper] -> IO ()
printMonkyLine i u [x] = do
  t <- getWrapperText i u x
  printf "%s\n" t

printMonkyLine i u (x:xs) = do
  t <- getWrapperText i u x
  printf "%s | " t
  printMonkyLine i u xs


mainLoop :: Int -> String -> [ModuleWrapper] -> IO()
mainLoop i u m = do
  printMonkyLine i u m
  hFlush stdout
  threadDelay 1000000
  mainLoop (i+1) u m



packMod :: Modules -> IO ModuleWrapper
packMod x = do
  ref <- newIORef ("" :: String)
  return (MWrapper x ref)


startLoop :: String -> IO [Modules] -> IO ()
startLoop u m = do
  n <- liftM (map packMod) m
  l <- sequence n
  mainLoop 0 u l


main :: IO()
main = do
  user <- getEffectiveUserName
  startLoop user getModules
