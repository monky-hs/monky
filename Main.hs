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
{-# LANGUAGE CPP #-}
{-|
Module      : monky
Description : A conky clone
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

This application has a subset of the functionality conky provides.

The goal of this application is to provide a standalone access to most system
values without spawning thousands of processes (looking at you conky).

It uses a similar configuration style to xmonad, i.e. the config file is a
haskell source file which uses library functions provided by the application.

The config file has to be placed at "~/.monky/monky.hs". Any valid Haskell
source file should work (it is compiled with ghc --make).

This executable compiles the configuration if needed, and execs into the main
executable.
-}
module Main
(main)
where

import Control.Monad (when)
import Data.List (isSuffixOf)
import Data.Map (Map)
import Monky.Version (getVersion)
import System.Directory (getDirectoryContents, createDirectoryIfMissing, setCurrentDirectory, getModificationTime, getHomeDirectory, removeFile)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitFailure)
import System.IO (withFile, IOMode(..), hPutStr, hGetLine)
import System.Posix.Process (executeFile)
import System.Process (system)

import qualified Data.Map as M

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

data Action
  = Create
  | Recompile
  | CheckRecompile
  | Execute
  | PrintUsage
  deriving (Ord, Show, Eq)

monkyPath :: IO String
monkyPath = flip (++) "/.monky" <$> getHomeDirectory

compilerFlags :: String
compilerFlags = "--make -fno-warn-orphans -odir build -hidir build"

changeDir :: IO ()
changeDir = do
  mdir <- monkyPath
  createDirectoryIfMissing False mdir
  setCurrentDirectory mdir


exampleFile :: String
exampleFile =
 "import Monky\n" ++
 "import Monky.Modules\n" ++
 "import Monky.CPU\n" ++
 "import Monky.Memory\n" ++
 "\n" ++
 "import Monky.Examples.CPU\n" ++
 "import Monky.Examples.Memory\n" ++
 "\n" ++
 "main :: IO()\n" ++
 "main = startLoop [pack 5 $getCPUHandle ScalingCur, pack 5 getMemoryHandle]"


createExample :: IO ()
createExample = withFile "monky.hs" WriteMode (`hPutStr` exampleFile)


createExampleIfMissing :: IO ()
createExampleIfMissing = do
  files <- getDirectoryContents "."
  when ("monky.hs" `notElem` files) createExample

createVersionFile :: ExitCode -> IO ()
createVersionFile (ExitFailure _) = putStrLn "Compiliation failed" >> exitFailure
createVersionFile ExitSuccess = withFile ".version" WriteMode (\file ->
   hPutStr file (show getVersion))


compile :: IO ()
compile = system ("ghc " ++ compilerFlags ++ " monky.hs -o monky") >>= createVersionFile


hasMonkyUpdated :: [FilePath] -> IO Bool
hasMonkyUpdated files =
  if ".version" `elem` files
    then withFile ".version" ReadMode (\file -> do
      l <- hGetLine file
      let (oldH, oldh, oldm, _) = read l :: (Int, Int, Int, Int)
      let (newH, newh, newm, _) = getVersion
      return (oldH < newH || oldh < newh || oldm < newm))
    else return True


needsRecompilation :: IO Bool
needsRecompilation = do
  files <- getDirectoryContents "."
  if "monky" `elem` files
    then if "monky.hs" `elem` files
      then do
        modT <- getModificationTime "monky"
        times <- sequence $map getModificationTime $filter (isSuffixOf ".hs") files
        monkyUpdated <- hasMonkyUpdated files
        return (maximum times > modT || monkyUpdated)
      else return False
    else do
      when ("monky.hs" `notElem` files) createExample
      return True


compileIfUpdated :: IO ()
compileIfUpdated = needsRecompilation >>= flip when compile


forceRecomp :: IO ()
forceRecomp = do
  files <- getDirectoryContents "."
  mapM_ removeFile (filter isCompiled files)
  when ("monky" `elem` files) (removeFile "monky")
  compile
  where isCompiled s = isSuffixOf ".hi" s || isSuffixOf ".o" s

parseArgs :: [String] -> Map Action Bool -> Map Action Bool
parseArgs ("--recompile":xs) m =
  parseArgs xs $ M.insert CheckRecompile False $M.insert Recompile True m
parseArgs ("-r":xs) m = parseArgs ("--recompile":xs) m
parseArgs ("--no-recompile":xs) m =
  parseArgs xs $ M.insert CheckRecompile False m
parseArgs ("-n":xs) m = parseArgs ("--no-recompile":xs) m
parseArgs ("--no-exec":xs) m = parseArgs xs $ M.insert Execute False m
parseArgs [] m = m
parseArgs _ _ = M.insert PrintUsage True M.empty


defaultActions :: Map Action Bool
defaultActions = M.fromList [(Create, True), (CheckRecompile, True), (Execute, True)]


getActionList :: [(Action, Bool)] -> [IO ()]
getActionList [] = []
getActionList ((_, False):xs) = getActionList xs
getActionList ((Recompile, True):xs) = forceRecomp:getActionList xs
getActionList ((CheckRecompile, True):xs) = compileIfUpdated:getActionList xs
getActionList ((Execute, True):xs) = executeMonky : getActionList xs
getActionList ((Create, True):xs) = createExampleIfMissing : getActionList xs
getActionList ((PrintUsage, True):xs) = printUsage : getActionList xs


getActions :: IO [IO ()]
getActions = do
  args <- getArgs
  let m = parseArgs args defaultActions
  return $getActionList $M.toAscList m

printUsage :: IO ()
printUsage = do
  putStrLn "Default: Create example if needed, compile if needed and run monky\n"
  putStrLn "--recompile/-r:     Force recompilation"
  putStrLn "--no-recompile/-n:  Don't recompile"
  putStrLn "--no-exec:          Don't execute main executable"

executeMonky :: IO ()
executeMonky = executeFile "./monky" False [] Nothing

main :: IO ()
main = do
  changeDir
  acts <- getActions
  sequence_ acts
