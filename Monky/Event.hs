{-# LANGUAGE CPP #-}
module Monky.Event
(startEventLoop)
where


import System.Posix.Types (Fd)
import GHC.Event (EventManager, getSystemEventManager, registerFd, evtRead)


#if MIN_VERSION_base(4,8,0)
#if MIN_VERSION_base(4,8,1)
-- base-4.8.1 exports this properly
import GHC.Event (Lifetime(..))
#else
-- recreation of the type for base-4.8.0
import Unsafe.Coerce (unsafeCoerce)
data Lifetime = OneShot | MultiShot
             deriving (Show, Eq)

--elSupremum :: Lifetime -> Lifetime -> Lifetime
--elSupremum OneShot OneShot = OneShot
--elSupremum _       _       = MultiShot
--{-# INLINE elSupremum #-}
--instance Monoid Lifetime where
--    mempty = OneShot
--    mappend = elSupremum
#endif
#endif


{- Main loop -}
getEvtMgr :: IO EventManager
getEvtMgr = do
  mgr <- getSystemEventManager
  case mgr of
    Just x -> return x
    Nothing -> error "Could not get IOManager, please compile with -threaded"



startEvents :: [(IO (), [Fd])] -> EventManager -> IO ()
-- No need to start anything, because 'loop' was removed for base-4.7+ we depend
-- on the system 'EventManager'
startEvents [] _ = return ()
startEvents ((act,fs):xs) m = do
  mapM_ addFd fs
  startEvents xs m
  where
#if MIN_VERSION_base(4,8,0)
#if MIN_VERSION_base(4,8,1)
  -- base-4.8.1 (and hopefully upwards) works correctly
    addFd fd = registerFd m (\_ _ -> act) fd evtRead MultiShot
#else 
  -- base-4.8.0 doesn't work with MultiShot so we register OneShots multiple times
    addFd fd = registerFd m (lambda fd) fd evtRead (unsafeCoerce OneShot)
    lambda fd _ _ = do
      _ <- act
      -- Since base-4.8.0.0s GHC.Event is broken...
      -- MultiShot does something similar internally so this should be ok
      _ <- registerFd m (lambda fd) fd evtRead (unsafeCoerce OneShot)
      return ()
#endif 
#else 
  -- Old interface before 4.8, works as expected
    addFd fd = registerFd m (\_ _ -> updateText mw u) fd evtRead
#endif 

startEventLoop :: [(IO (), [Fd])] -> IO ()
startEventLoop xs =  getEvtMgr >>= startEvents xs
