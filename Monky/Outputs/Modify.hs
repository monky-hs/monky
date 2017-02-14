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
Module      : Monky.Output.Modify
Description : Monky module that modifies other outputs behaviour
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Outputs.Modify
  ( ModifyOutput
  , getModifyOutput
  )
where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Monky.Modules

-- |Handle for modifying a line before it's passed to the output
data ModifyOutput a = MH ([[MonkyOut]] -> [[MonkyOut]]) a

-- |Get a handle to purely modify a line
getModifyOutput :: ([[MonkyOut]] -> [[MonkyOut]]) -> IO a -> IO (ModifyOutput a)
getModifyOutput = fmap . MH

instance MonkyOutput a => MonkyOutput (ModifyOutput a) where
    doLine (MH f x) line = doLine x $ f line
