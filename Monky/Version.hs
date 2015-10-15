{-# LANGUAGE TemplateHaskell #-}
module Monky.Version
where

import Monky.VersionTH

getVersion :: (Int, Int, Int, Int)
getVersion = $versionTH
