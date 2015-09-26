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

import Monky (getVersion)
import Data.List (isSuffixOf)
import Control.Monad (when)
import Control.Applicative ((<$>))
import System.Directory (getDirectoryContents, createDirectoryIfMissing, setCurrentDirectory, getModificationTime, getHomeDirectory)
import System.Exit (ExitCode(..), exitFailure)
import System.Process (system)
import System.Posix.Process (executeFile)
import System.IO (withFile, IOMode(..), hPutStr, hGetLine)

monkyPath :: IO String
monkyPath = flip (++) "/.monky" <$> getHomeDirectory

compilerFlags :: String
compilerFlags = "--make -fno-warn-orphans"

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
createExample = withFile "monky.hs" WriteMode (flip hPutStr exampleFile)


createVersionFile :: ExitCode -> IO ()
createVersionFile (ExitFailure _) = putStrLn "Compiliation failed" >> exitFailure
createVersionFile ExitSuccess = withFile ".version" WriteMode (\file ->
   hPutStr file (show getVersion))


compile :: IO ()
compile = system ("ghc " ++ compilerFlags ++ " monky.hs -o monky") >>= createVersionFile


hasMonkyUpdated :: [FilePath] -> IO Bool
hasMonkyUpdated files = do
  if ".version" `elem` files
    then withFile ".version" ReadMode (\file -> do
      l <- hGetLine file
      let (oldH, oldh, oldm, _) = (read l :: (Int, Int, Int, Int))
      let (newH, newh, newm, _) = getVersion
      return (oldH < newH || oldh < newh || oldm < newm))
    else return True


compileIfUpdated :: IO ()
compileIfUpdated = do
  files <- getDirectoryContents "."
  if "monky" `elem` files
    then when ("monky.hs" `elem` files) $do
      modT <- getModificationTime "monky"
      times <- sequence $map getModificationTime $filter (isSuffixOf ".hs") files
      monkyUpdated <- hasMonkyUpdated files
      when (maximum times > modT || monkyUpdated) compile
    else do
      when ("monky.hs" `notElem` files) (createExample >> (putStrLn $show files))
      compile



main :: IO ()
main = do
  changeDir
  compileIfUpdated
  executeFile "./monky" False [] Nothing
