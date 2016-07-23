module Monky.Examples.Plain
  ( MonkyList(..)
  , getPlain
  )
where

import Monky.Modules

newtype MonkyList = MonkyList [MonkyOut]

instance PollModule MonkyList where
  getOutput (MonkyList x) = return x

getPlain :: [MonkyOut] -> IO MonkyList
getPlain = return . MonkyList
