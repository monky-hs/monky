{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Examples.Network
Description : An example module instance for the network module
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Examples.Network 
  ( getNetworkHandles'
  , getStaticHandle
  , getDynamicHandle

  , NetworkDynHandle
  , NetworkStaticHandle
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef (readIORef)

import Monky.Examples.Utility
import Monky.Modules
import Monky.Network

import qualified Monky.Network.Dynamic as D
import qualified Monky.Network.Static as S

data NetworkHandles' = NH' Text NetworkHandles

-- |Handle for dynamic network handling
data NetworkDynHandle = DH Text D.UHandles
-- |HAndle for a staticly configured network interface
data NetworkStaticHandle = SH Text S.NetworkHandle

-- |Actually get the network handle
getNetworkHandles'
  :: Text -- ^The string to use when the network is disconnected
  -> [String] -- ^The Network adapters to monitor
  -> IO NetworkHandles'
getNetworkHandles' e = fmap (NH' e) . getNetworkHandles

-- |Get a handle for a static network interface
getStaticHandle
  :: Text -- ^The string to use when network is disconnected
  -> String -- ^Name of the network interface
  -> IO NetworkStaticHandle
getStaticHandle e = fmap (SH e) . S.getNetworkHandle

-- |Get a handle that will update with new or disappearing network interfaces
getDynamicHandle
  :: Text -- ^The string to use when network is disconnected
  -> (String -> Bool) -- ^The filter function for the dynamic handle
  -> IO NetworkDynHandle
getDynamicHandle e = fmap (DH e) . D.getUHandles



{- 2.0 STUFF -}
{- Network Module -}

formatNNetworkText :: Text -> Maybe (Int, Int) -> Text
formatNNetworkText e Nothing = e
formatNNetworkText _ (Just (r, w)) =
  convertUnit r "B" "k" "M" "G" `T.append` (' ' `T.cons` convertUnit w "B" "k" "M" "G")


getNNetworkText :: Text -> NetworkHandles -> IO [MonkyOut]
getNNetworkText e nh = do
  nv <- getReadWriteMulti nh
  return [MonkyPlain $ formatNNetworkText e nv]


getNDNetworkText :: Text -> D.Handles -> IO [MonkyOut]
getNDNetworkText e nh = do
  nv <- D.getMultiReadWrite nh
  return [MonkyPlain $ formatNNetworkText e nv]


instance PollModule NetworkDynHandle where
  getOutput (DH e h) = getNDNetworkText e =<< readIORef (fst h)

instance PollModule NetworkHandles' where
  getOutput (NH' e h) = getNNetworkText e h

instance PollModule NetworkStaticHandle where
  getOutput (SH e h) = do
    rates <- S.getReadWrite h
    return [MonkyPlain $ formatNNetworkText e rates]

