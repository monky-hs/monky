{-
    Copyright 2015,2016 Markus Ongyerth, Stephan Guenther

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
  ( main
  )
where

import GHC.IO.Handle (hDuplicate)
import Monky.Version (getVersion)
import Control.Monad (when, unless)
import Data.List (isSuffixOf, nub, sort)
import System.Directory
import System.Exit (ExitCode(..), exitFailure)
import System.IO (withFile, IOMode(..), hPutStr, hPutStrLn, stderr)
import System.Posix.Process (executeFile)
import System.Process (shell, waitForProcess, CreateProcess(..), createProcess, StdStream(..))
import Data.Monoid ((<>))

import Options.Applicative

data Action
  = Create
  | ForceCompile
  | Recompile
  | Execute
  deriving (Ord, Show, Eq)

getActions :: Config -> [Action]
getActions c =
  (if confCompile c then [ForceCompile] else []) ++
  (if confGenerate c then [Create] else []) ++
  [Recompile, Execute]

data Config = Config
  { monkyDir :: String
  , exeName :: String

  , confCompile :: Bool
  , confNoCompile :: Bool

  , confNoExec :: Bool

  , confGenerate :: Bool
  , confNoGenerate :: Bool
  } deriving (Show)

-- Takes the current $HOME and a config dir, replaces '~' at beginning of paths
updatePath :: Config -> String -> Config
updatePath c home = c { monkyDir = monkyDir' }
  where monkyDir' = case monkyDir c of
                     ('~':xs) -> home ++ xs
                     (xs) -> xs

getConfigP :: Parser Config
getConfigP = Config <$>
   strOption (long "monky-dir" <> help "The directory monky resides in. Defaults to ~/.monky" <> short 'd' <> value "~/.monky") <*>
   strOption (long "monky-exe" <> help "The name of the executable to be created/used." <> short 'e' <> value "monky.exe") <*>
   switch (long "compile" <> help "Force compilation, even if helper thinks it's not needed" <> short 'c') <*>
   switch (long "no-compile" <> help "Do not try to compile the executable" <> short 'n') <*>
   switch (long "no-exec" <> help "Do not execute the compiled executable") <*>
   switch (long "generate-example" <> help "Force example generation. This will overwrite existing config!") <*>
   switch (long "no-generate-example" <> help "Don't generate the example, even if required")

monkyDesc :: String
monkyDesc = concat
  [ "Monky version: " ++ show getVersion ++ "."
  , "Monky helper to compile and/or execute the real monky binary"
  , "This executable will call ghc to (re)compile the main monky.exe"
  , "Then it will exec monky.exe which in turn will generate the output"
  , "This file is a simple helper/wrapper. You can execute monky.exe without."
  ]

getConfig :: IO Config
getConfig = do
  conf <- execParser opts
  updatePath conf <$> getHomeDirectory
  where opts = info (helper <*> getConfigP)
                    (fullDesc <>
                      header monkyDesc)


compilerFlags :: String
compilerFlags = "--make -XOverloadedStrings -odir build -hidir build -O -with-rtsopts=-V0"


changeDir :: Config -> IO ()
changeDir c = do
  createDirectoryIfMissing False $ monkyDir c
  setCurrentDirectory $ monkyDir c


exampleFile :: String
exampleFile = unlines
 [ "import Monky"
 , "import Monky.Modules"
 , ""
 , "import Monky.Examples.CPU"
 , "import Monky.Examples.Memory"
 , ""
 , "import Monky.Outputs.Ascii"
 , ""
 , "main :: IO ()"
 , "main = startLoop getAsciiOut"
 , "  [ pollPack 1 $ getRawCPU"
 , "  , pollPack 1 getMemoryHandle"
 , "  ]"
 ]

createExample :: Config -> IO ()
createExample c =
  unless (confNoGenerate c)$ withFile "monky.hs" WriteMode (`hPutStr` exampleFile)


shouldCreate :: IO Bool
shouldCreate = do
  files <- getDirectoryContents "."
  return $ "monky.hs" `notElem` files

runGHC :: Config -> IO ExitCode
runGHC c = do
    let com = "ghc " ++ compilerFlags ++ " monky.hs -o " ++ exeName c
    let proc = shell com
    file <- hDuplicate stderr
    (_, _, _, h) <- createProcess proc { std_out = UseHandle file }
    waitForProcess h


compile :: Config -> IO ()
compile c = unless (confNoCompile c) $ do
  exists <- doesFileExist "monky.hs"
  when exists $ do
    ret <- runGHC c
    case ret of
      (ExitFailure _) -> do
        hPutStrLn stderr "Compilation failed"
        exitFailure
      ExitSuccess -> return ()


forceRecomp :: Config -> IO ()
forceRecomp c = unless (confNoCompile c) $ do
  files <- getDirectoryContents "."
  mapM_ removeFile (filter isCompiled files)
  when (exeName c `elem` files) (removeFile (exeName c))
  when ("build" `elem` files) (removeDirectoryRecursive "build")
  where isCompiled s = isSuffixOf ".hi" s || isSuffixOf ".o" s


executeMonky :: Config -> IO ()
executeMonky c = unless (confNoExec c) $ executeFile ("./" ++ exeName c) False [] Nothing


execAction :: Action -> (Config -> IO ())
execAction Create = createExample
execAction ForceCompile = forceRecomp
execAction Recompile = compile
execAction Execute = executeMonky


main :: IO ()
main = do
  conf <- getConfig
  changeDir conf
  should <- shouldCreate
  let actions = getActions conf
  let acts = map execAction . nub . sort $ if should then Create:actions else actions
  mapM_ ($conf) acts
