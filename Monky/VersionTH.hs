{-# LANGUAGE CPP #-}
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

versionTH :: Q Exp
versionTH = do
  content <- lines <$> runIO (readFile "monky.cabal")
  let parts = map read $splitAtEvery "." $getVersionString content
  returnQ . TupE $map (LitE . IntegerL) parts
  where getVLine = head . filter ("version:" `isPrefixOf`)
        getVersionString = flip (!!) 1 . words . getVLine
