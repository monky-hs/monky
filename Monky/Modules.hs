{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : Monky.Modules
Description : The module definition used by 'startLoop'
Maintainer  : ongy, moepi
Stability   : experimental
Portability : Linux

This module provides the 'Module' class which is used to define a 'Monky'
compatible module.
-}
module Monky.Modules
(Modules(..), Module(..), pack)
where

import System.Posix.Types (Fd)

-- |A wrapper around module instances so they can be put into a list.
data Modules = forall a . Module a => MW a Int
-- |The type class for modules
class Module a where
    getText :: String -- ^The current user
            -> a -- ^The handle to this module
            -> IO String -- ^The text segment that should be displayed for this module
    getFDs :: a -- ^The handle to this module
           -> IO [Fd] -- ^The 'Fd's to listen on for events
    getFDs _ = return []

-- |Function to make packaging modules easier
pack :: Module a
     => Int -- ^The refresh rate for this module
     -> IO a -- ^The function to get a module (get??Handle)
     -> IO Modules -- ^The packed module ready to be given to 'startLoop'
pack i a = a >>= (return . flip MW i)
