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
{-# LANGUAGE CPP #-}
{-|
Module      : Monky.VersionTH
Description : The TemplateHaskell splice required by Monky.Version
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.VersionTH
where

import Data.List (isPrefixOf)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Monky.Utility

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

-- |Get the version string from monky.cabal and convert it to a 4tupel
versionTH :: Q Exp
versionTH = do
  content <- lines <$> runIO (readFile "monky.cabal")
  let parts = map read $splitAtEvery "." $getVersionString content
  returnQ . TupE $map (LitE . IntegerL) parts
  where getVLine = head . filter ("version:" `isPrefixOf`)
        getVersionString = flip (!!) 1 . words . getVLine
