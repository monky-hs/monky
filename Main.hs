{-|
Module      : monky
Description : A conky clone
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

This application has a subset of the functionality conky proveds.

The goal of this application is to provide a standalone access to most system
values without spawning thousands of processes (looking at you conky).
The general design is to create handles at the start which encapsule state and
file descriptors which are used to get the system status.
-}
module Main
(main)
where

import Data.List (isSuffixOf)
import Control.Monad (when)
import System.Directory
import System.Process (system)
import System.Posix.Process (executeFile)
import System.IO (withFile, IOMode(WriteMode), hPutStr)

changeDir :: IO ()
changeDir = do
  home <- getHomeDirectory
  let mdir = home ++ "/.monky"
  createDirectoryIfMissing False mdir
  setCurrentDirectory mdir


exampleFile :: String
exampleFile =
 "import Monky\n" ++
 "import Monky.Modules\n" ++
 "import Monky.CPU\n" ++
 "import Monky.Memory\n" ++
 "\n" ++
 "main :: IO()\n" ++
 "main = startLoop [pack $getCPUHandle ScalingCur, pack getMemoryHandle ]"

createExample :: IO ()
createExample = withFile "monky.hs" WriteMode (flip hPutStr exampleFile)

compile :: IO ()
compile = system "ghc --make monky.hs -o monky" >> return ()

compileIfUpdated :: IO ()
compileIfUpdated = do
  files <- getDirectoryContents "."
  if "monky" `elem` files
    then when ("monky.hs" `elem` files) $do
      modT <- getModificationTime "monky"
      times <- sequence $map getModificationTime $filter (isSuffixOf ".hs") files
      when (maximum times > modT) compile
    else do
      when ("monky.hs" `notElem` files) (createExample >> (putStrLn $show files))
      compile


main :: IO ()
main = do
  changeDir
  compileIfUpdated
  executeFile "./monky" False [] Nothing
