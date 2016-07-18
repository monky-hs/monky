{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
module Monky.Outputs.Fallback
  ( WrapOuts
  , getFallbackOut
  )
where

import System.IO
import GHC.IO.Encoding
import Monky.Modules

import Monky.Outputs.Ascii
import Monky.Outputs.Utf8

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

data WrapOuts = forall a . MonkyOutput a => WO a

instance MonkyOutput WrapOuts where
  doLine (WO o) = doLine o

chooseTerminalOut :: IO WrapOuts
chooseTerminalOut = do
  l <- getLocaleEncoding
  if textEncodingName l == "UTF-8"
    then return . WO $ getUtf8Out
    else return . WO $ getAsciiOut

getFallbackOut :: MonkyOutput a => IO a -> IO WrapOuts
getFallbackOut o = do
  e <- hIsTerminalDevice stdout
  if e
    then chooseTerminalOut
    else WO <$> o
