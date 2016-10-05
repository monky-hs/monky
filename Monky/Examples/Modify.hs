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
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.Examples.Modify
Description : Monky module that modifies other modules output
Maintainer  : ongy
Stability   : testing
Portability : Linux

Example that limits MPD song title to 20 characters

@
import Data.Text as T

modifyMPD :: [MonkyOut] -> [MonkyOut]
modifyMPD orig@(MonkyPlain xs:ys) =
  if T.length xs > 20
     then MonkyPlain (T.take 17 xs `T.append` "..."):ys
     else orig


evtPack    $ getModifyHandle modifyMPD $ getMPDHandle "127.0.0.1" "6600"
@

-}
module Monky.Examples.Modify
  ( ModifyHandle
  , getModifyHandle

  , IOModifyHandle
  , getIOModifyHandle

  , changeImage
  )
where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Monky.Modules

-- |Handle for duing pure modification of module output
data ModifyHandle a = MH ([MonkyOut] -> [MonkyOut]) a

-- |Get a handle to purely modify another modules output
getModifyHandle :: ([MonkyOut] -> [MonkyOut]) -> IO a -> IO (ModifyHandle a)
getModifyHandle = fmap . MH

instance PollModule a => PollModule (ModifyHandle a) where
  initialize (MH _ m) = initialize m
  getOutput (MH fun m) = fun <$> getOutput m

instance EvtModule a => EvtModule (ModifyHandle a) where
  startEvtLoop (MH fun m) f = startEvtLoop m (f . fun)


-- |Handle for modifying output in IO monad
data IOModifyHandle a = IOMH ([MonkyOut] -> IO [MonkyOut]) a

-- |Get a handle that can modify another handles output in the IO monad
getIOModifyHandle :: ([MonkyOut] -> IO [MonkyOut]) -> IO a -> IO (IOModifyHandle a)
getIOModifyHandle fun = fmap (IOMH fun)

instance PollModule a => PollModule (IOModifyHandle a) where
  initialize (IOMH _ m) = initialize m
  getOutput (IOMH fun m) = fun =<< getOutput m

instance EvtModule a => EvtModule (IOModifyHandle a) where
  startEvtLoop (IOMH fun m) f = startEvtLoop m (\e -> f =<< fun e)

-- |Change the replacemant char in an monky image (if your font doesn't support it), returns initial value if not an image
changeImage :: Char -> MonkyOut -> MonkyOut
changeImage c (MonkyImage x _) = MonkyImage x c
changeImage _ x                = x
