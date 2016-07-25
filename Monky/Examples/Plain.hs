{-|
Module : Monky.Examples.Plain
Description: Print some constant values
Maintainer: ongy
Stability: testing
Portability: linux
-}
module Monky.Examples.Plain
  ( MonkyList(..)
  , getPlain
  )
where

import Monky.Modules

-- |The type for this functionality
newtype MonkyList = MonkyList [MonkyOut]

instance PollModule MonkyList where
  getOutput (MonkyList x) = return x

instance EvtModule MonkyList where
  startEvtLoop (MonkyList xs) a = a xs

-- |Get a 'MonkyList' for familiar syntax
getPlain :: [MonkyOut] -> IO MonkyList
getPlain = return . MonkyList
