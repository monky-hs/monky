{-
    Copyright 2016 Markus Ongyerth

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
{-# LANGUAGE CPP #-}
{-|
Module : Monky.Prepend
Description: Prepend something to a module
Maintainer: ongy
Stability: testing
Portability: linux

The *Prepend/Append functions can be used instead of pollPack/evtPack.
If you want to chain a prepended module into something else, use the raw Constructors.
@
packPrepend [MonkyPlain "CPU"] 1 $ getCPUHandle ScalingCur
pack 1 $ Prep [MonkyPlain "CPU"] <$> getCPUHandle ScalingCur
@
-}

module Monky.Examples.Prepend
  ( PrepHandle(..)
  , PostHandle(..)

  , packPrepend
  , packAppend

  , EvtPrepHandle(..)
  , EvtPostHandle(..)

  , evtPrepend
  , evtAppend
  )
where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Monky.Modules

-- |The handle used by this module, contains underlying module and string
data PrepHandle = forall m . PollModule m => Prep [MonkyOut] m

-- |The handle used by this module, contains underlying module and string
data PostHandle = forall m . PollModule m => Post [MonkyOut] m

instance PollModule PrepHandle where
  getOutput  (Prep t m) = (t ++) <$> getOutput m
  initialize (Prep _ m) = initialize m

instance PollModule PostHandle where
  getOutput  (Post t m) = (++t) <$> getOutput m
  initialize (Post _ m) = initialize m

-- TODO: Add EvtModule instance

{-| Create a module that should be prepended with some string

This allows you to prepend an instance of a module with a fixed
String.

For usage look at 'pollPack'.
-}
packPrepend :: PollModule a
            => [MonkyOut] -- ^The String to prepend
            -> Int -- ^The refresh rate for this module
            -> IO a -- ^The function to get the module
            -> IO Modules -- ^The returned handle
packPrepend x i m = pollPack i (Prep x <$> m)

{-| Create a module that should be appended with some string

This allows you to append an instance of a module with a fixed
String.

For usage look at 'pollPack'.
-}
packAppend :: PollModule a
           => [MonkyOut] -- ^The String to prepend
           -> Int -- ^The refresh rate for this module
           -> IO a -- ^The function to get the module
           -> IO Modules -- ^The returned handle
packAppend x i m = pollPack i (Post x <$> m)


-- |'EvtModule' prepend type
data EvtPrepHandle = forall m . EvtModule m => EvtPrep [MonkyOut] m
-- |'EvtModule' append type
data EvtPostHandle = forall m . EvtModule m => EvtPost [MonkyOut] m

instance EvtModule EvtPrepHandle where
  startEvtLoop (EvtPrep x m) a =
    startEvtLoop m (\xs -> a (x ++ xs))

instance EvtModule EvtPostHandle where
  startEvtLoop (EvtPost x m) a =
    startEvtLoop m (\xs -> a (xs ++ x))

-- |'packPrepend' for 'EvtModule's
evtPrepend :: EvtModule a
           => [MonkyOut]
           -> IO a
           -> IO Modules
evtPrepend x m = evtPack $ EvtPrep x <$> m

-- |'packAppend' for 'EvtModule's
evtAppend :: EvtModule a
          => [MonkyOut]
          -> IO a
          -> IO Modules
evtAppend x m = evtPack $ EvtPost x <$> m

