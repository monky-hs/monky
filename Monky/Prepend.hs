{-|
Module : Monky.Prepend
Description: Prepend something to a module
Maintainer: ongy
Stability: testing
Portability: linux
-}

module Monky.Prepend
  ( PrepHandle
  , packPrepend
  )
where

import Monky.Modules

data PrepHandle = Prep String Modules


instance Module PrepHandle where
  getText u (Prep x (MW a _)) = (++) x <$> getText u a
  getFDs (Prep _ (MW a _)) = getFDs a
  getEventText fd u (Prep x (MW a _)) = (++)x <$> getEventText fd u a
  setupModule (Prep _ (MW a _)) = setupModule a
  getTextFailable u (Prep x (MW a _)) = do
    ret <- getTextFailable u a
    return ((++) x <$> ret)
  getEventTextFailable fd u (Prep x (MW a _)) = do
    ret <- getEventTextFailable fd u a
    return ((++) x <$> ret)
  recoverModule (Prep _ (MW a _)) = recoverModule a

packPrepend :: Module a
            => String -- ^The String to prepend
            -> Int -- ^The refresh rate for this module
            -> IO a -- ^The function to get the module
            -> IO Modules -- ^The returned handle
packPrepend x i m = pack i (Prep x <$> pack 0 m)
