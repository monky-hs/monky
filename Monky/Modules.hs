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
    {- |This function is used instead of 'getText' for event triggerd updates.

      The default implementation mappes this to 'getText'
    -}
    getEventText :: Fd -- ^The fd that triggered an event
                 -> String -- ^The current user
                 -> a -- ^The handle to this module
                 -> IO String -- ^The text segment that should be displayed
    getEventText _ = getText

-- |Function to make packaging modules easier
pack :: Module a
     => Int -- ^The refresh rate for this module
     -> IO a -- ^The function to get a module (get??Handle)
     -> IO Modules -- ^The packed module ready to be given to 'startLoop'
pack i = fmap (flip MW i)
