{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module Monky.Examples.Combine
  ( getCombi
  , CombineHandle
  , packCombi
  )
where

import Monky.Modules
import System.IO.Unsafe (unsafePerformIO)

data ComboMod = forall a . PollModule a => CM a

class CombineType t where
  spr :: [ComboMod] -> t

instance (PollModule t, CombineType r) => CombineType (IO t -> r) where
  spr xs = \m -> spr (CM (unsafePerformIO m):xs)

getCombiOut :: ComboMod -> IO [MonkyOut]
getCombiOut (CM h) = getOutput h

newtype CombineHandle = CH [ComboMod]

instance CombineType CombineHandle where
  spr = CH . reverse

instance PollModule CombineHandle where
  getOutput (CH xs) = fmap concat $ mapM getCombiOut xs

getCombi :: CombineType t => t
getCombi = spr []

packCombi :: Int -> CombineHandle -> IO Modules
packCombi i = pollPack i . return
