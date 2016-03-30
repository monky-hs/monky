{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
  )
where

import Data.IORef (readIORef)

import Monky.Utility
import Monky.Modules
import Monky.Network

import qualified Monky.Network.Dynamic as D
import qualified Monky.Network.Static as S

data NetworkHandles' = NH' String NetworkHandles
data NetworkDynHandle = DH String D.UHandles
data NetworkStaticHandle = SH String S.NetworkHandle

instance Module NetworkHandles' where
  getText u (NH' e h) = getNetworkText e u h

-- |Example instance for network module
instance Module NetworkHandles where
  getText = getNetworkText "Network Off"

instance Module S.NetworkHandle where
  getText _ h = do
    rates <- S.getReadWrite h
    return $ formatNetworkText "Network off" rates

instance Module NetworkStaticHandle where
  getText _ (SH e h) = do
    rates <- S.getReadWrite h
    return $ formatNetworkText e rates

-- No real reason for this to exist
instance Module D.Handles where
  getText = getDNetworkText "Network Off"

instance Module D.UHandles where
  getText u h = getDNetworkText "Network Off" u =<< readIORef (fst h)

instance Module NetworkDynHandle where
  getText u (DH e h) = getDNetworkText e u =<< readIORef (fst h)

-- |Actually get the network handle
getNetworkHandles'
  :: String -- ^The string to use when the network is disconnected
  -> [String] -- ^The Network adapters to monitor
  -> IO NetworkHandles'
getNetworkHandles' e = fmap (NH' e) . getNetworkHandles

getStaticHandle
  :: String -- ^The string to use when network is disconnected
  -> String -- ^Name of the network interface
  -> IO NetworkStaticHandle
getStaticHandle e = fmap (SH e) . S.getNetworkHandle

getDynamicHandle
  :: String -- ^The string to use when network is disconnected
  -> (String -> Bool) -- ^The filter function for the dynamic handle
  -> IO NetworkDynHandle
getDynamicHandle e = fmap (DH e) . D.getUHandles


{- Network Module -}
formatNetworkText :: String -> Maybe (Int, Int) -> String
formatNetworkText e Nothing = e
formatNetworkText _ (Just (r, w)) =
  convertUnit r "B" "k" "M" "G" ++ ' ':convertUnit w "B" "k" "M" "G"


getNetworkText :: String -> String -> NetworkHandles -> IO String
getNetworkText e _ nh = do
  nv <- getReadWriteMulti nh
  return (formatNetworkText e nv)


getDNetworkText :: String -> String -> D.Handles -> IO String
getDNetworkText e _ nh = do
  nv <- D.getMultiReadWrite nh
  return $ formatNetworkText e nv

