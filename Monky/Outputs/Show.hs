{-|
Module      : Monky.Outputs.Show
Description : Output module for storing
Maintainer  : ongy
Stability   : testing
Portability : Linux
-}
module Monky.Outputs.Show
where

import System.IO (hFlush, stdout)
import Monky.Modules

-- |The output handle for dzen2 pipe
data ShowOutput = ShowOutput

instance MonkyOutput ShowOutput where
  doLine _ xs = do
    putStr $ show xs
    putStr "\n"
    hFlush stdout

-- |Get an output handle for ascii formatting
getShowOut :: IO ShowOutput
getShowOut = return ShowOutput
