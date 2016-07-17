module Monky.Examples.Utility
 ( loopFd
 )
where


import Control.Concurrent (threadWaitRead)
import Data.IORef (IORef, atomicWriteIORef)
import System.Posix.Types (Fd)
import Monky.Modules


-- |Utility Function for eventing modules
loopFd
  :: h -- ^Some kind of module handle
  -> Fd -- ^The FD to block on
  -> IORef [MonkyOut] -- ^The IORef passed to startEvtLoop
  -> (h -> IO [MonkyOut]) -- ^The function to generate the output
  -> IO () -- ^This will loop for you
loopFd h fd r f = do
  threadWaitRead fd
  out <- f h
  atomicWriteIORef r out
  loopFd h fd r f
