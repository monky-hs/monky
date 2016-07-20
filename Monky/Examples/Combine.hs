{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module : Monky.Examples.Combine
Description: Combine multiple modules into one (to avoid seperators)
Maintainer: ongy
Stability: testing
Portability: linux


Nearly equivalend example (image is missing):

@
pollPack 1 $ getCPUHandle' ScalingCur
packCombi 1 $ getCombi (getFreqHandle ScalingCur) getRawCPU getTempHandle'
pollPack 1 $ combine (getFreqHandle ScalingCur) `add` getRawCPU `add` getTempHandle'
@

'combine' is the saner and better version, 'getCombi' mostly exists because I wanted to try making it.
'combine' and 'add' can also be chained into pre/post packaging.

It's important to use 'packCombi' with 'getCombi', instead of pollPack, since `getCombi`
has an unusual type to support a variable number of arguments.
-}
module Monky.Examples.Combine
  ( getCombi
  , CombineHandle
  , packCombi

  , combine
  , add
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

combiInit :: ComboMod -> IO ()
combiInit (CM m) = initialize m

newtype CombineHandle = CH [ComboMod]

instance CombineType CombineHandle where
  spr = CH . reverse

instance PollModule CombineHandle where
  getOutput (CH xs) = fmap concat $ mapM getCombiOut xs
  initialize (CH xs) = mapM_ combiInit xs

getCombi :: CombineType t => t
getCombi = spr []

packCombi :: Int -> CombineHandle -> IO Modules
packCombi i = pollPack i . return

combine :: PollModule m => IO m -> IO CombineHandle
combine = fmap (CH . return . CM)

add :: PollModule m => IO CombineHandle -> IO m -> IO CombineHandle
add cur iom = do
  (CH xs) <- cur
  m <- iom
  return $ CH (xs ++ [CM m])
