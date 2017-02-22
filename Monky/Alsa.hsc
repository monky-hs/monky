{-
    Copyright 2015 Markus Ongyerth, Stephan Guenther

    This file is part of Monky.

    Monky is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Monky is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Monky.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Monky.Alsa
Description : Allows acces to information about the alsa sound system
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides access to basic audio information provided by the alsa
audio system.
This MAY work with pulse, but will report useless/inaccurate values.
-}
module Monky.Alsa
  ( VOLHandle
  , destroyVOLHandle
  , getMute
  , getVolumeRaw
  , getVolumePercent
  , updateVOLH
  , getVOLHandle
  , isLoaded
  , getPollFDs
  )
where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.IORef
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..), CShort, CLong)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Monky.Template
import System.Posix.Types

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

#include <poll.h>

data PollFD = POLLFD CInt CShort CShort

instance Storable PollFD where
  sizeOf _ = #{size struct pollfd}
  alignment _ = alignment (undefined :: CLong)
  peek p = do
    fd <- #{peek struct pollfd, fd} p
    events <- #{peek struct pollfd, events} p
    revents <- #{peek struct pollfd, revents} p
    return (POLLFD fd events revents)
  poke p (POLLFD fd events revents) = do
    #{poke struct pollfd, fd} p fd
    #{poke struct pollfd, events} p events
    #{poke struct pollfd, revents} p revents

type PollFDPtr = Ptr PollFD

liftExceptT :: ((a -> m (Either e b)) -> m (Either e b)) -> (a -> ExceptT e m b) -> ExceptT e m b
liftExceptT g f = ExceptT (g (runExceptT . f))

data RegOpt
data MClass

data Mixer
type MixerHandle = Ptr Mixer
type MixerHandleAlloc = Ptr MixerHandle

data Sid
type SidHandle = Ptr Sid
type SidHandleAlloc = Ptr SidHandle

data Elem
type ElemHandle = Ptr Elem

importLib "LibAlsa" "libasound.so.2"
  [ ("mixer_open", "snd_mixer_open", "MixerHandleAlloc -> Int -> IO CInt")
  , ("mixer_attach", "snd_mixer_attach", "MixerHandle -> CString -> IO CInt")
  , ("mixer_register", "snd_mixer_selem_register", "MixerHandle -> Ptr RegOpt -> Ptr MClass -> IO CInt")
  , ("mixer_load", "snd_mixer_load", "MixerHandle -> IO CInt")

  , ("sid_sindex", "snd_mixer_selem_id_set_index", "SidHandle -> CInt -> IO ()")
  , ("sid_sname", "snd_mixer_selem_id_set_name", "SidHandle -> CString -> IO ()")
  , ("sid_alloc", "snd_mixer_selem_id_malloc", "SidHandleAlloc -> IO CInt")
  , ("sid_free", "snd_mixer_selem_id_free", "SidHandle -> IO ()")

  , ("elem_gvrange", "snd_mixer_selem_get_playback_volume_range", "ElemHandle -> Ptr CInt -> Ptr CInt -> IO CInt")
  , ("elem_gvol", "snd_mixer_selem_get_playback_volume", "ElemHandle -> CInt -> Ptr CInt -> IO CInt")
  , ("elem_gmute", "snd_mixer_selem_get_playback_switch", "ElemHandle -> CInt -> Ptr CInt -> IO CInt")

  , ("elem_find", "snd_mixer_find_selem", "MixerHandle -> SidHandle -> IO ElemHandle")
  , ("mixer_handle_events", "snd_mixer_handle_events", "MixerHandle -> IO ()")
  , ("get_pdescs", "snd_mixer_poll_descriptors", "MixerHandle -> PollFDPtr -> CInt -> IO CInt")
  , ("get_pdescc", "snd_mixer_poll_descriptors_count", "MixerHandle -> IO CInt")

  , ("mixer_close", "snd_mixer_close", "MixerHandle -> IO Int")
  ]




getPollDescs :: MixerHandle -> LibAlsa -> IO [CInt]
getPollDescs h l = do
  count <- get_pdescc l h
  allocaArray (fromIntegral count) $ \ptr -> do
    c2 <- get_pdescs l h ptr count
    if count == c2
      then map  (\(POLLFD fd _ _) -> fd) <$> (peekArray (fromIntegral c2) ptr)
      else error "libalsa returned more (or less) fds than it adveritses!"


openMixer :: LibAlsa -> ExceptT Int IO MixerHandle
openMixer l = liftExceptT alloca $ \ptr -> do
  rval <- liftIO (mixer_open l ptr 0)
  if rval < 0
     then throwE $ fromIntegral rval
     else liftIO (peek ptr)


mixerAttach :: MixerHandle -> String -> LibAlsa -> ExceptT Int IO ()
mixerAttach handle card l = do
  rval <- liftIO (withCString card $ mixer_attach l handle)
  if rval < 0
     then throwE $ fromIntegral rval
     else liftIO (return ())


mixerRegister :: MixerHandle -> LibAlsa -> ExceptT Int IO ()
mixerRegister handle l = do
  rval <- liftIO (mixer_register l handle nullPtr nullPtr)
  if rval < 0
     then throwE $ fromIntegral rval
     else liftIO (return ())


mixerLoad :: MixerHandle -> LibAlsa -> ExceptT Int IO ()
mixerLoad handle l = do
  rval <- liftIO (mixer_load l handle)
  if rval < 0
     then throwE $ fromIntegral rval
     else liftIO (return ())


withSid :: LibAlsa -> (SidHandle -> IO a) -> IO a
withSid l fun = alloca $ \ptr -> do
  rval <- sid_alloc l ptr
  if rval < 0
    then error "Failed to allocate sid"
    else do
      handle <- peek ptr
      comp <- fun handle
      sid_free l handle
      return comp


sidSet :: SidHandle -> Int -> String -> LibAlsa -> IO ()
sidSet handle index name l = do
  withCString name $ sid_sname l handle
  sid_sindex l handle $ fromIntegral index


getElem :: MixerHandle -> String -> Int -> LibAlsa -> IO ElemHandle
getElem handle name index l = withSid l $ \sid -> do
  sidSet sid index name l
  elem_find l handle sid


isMute :: ElemHandle -> LibAlsa -> IO Bool
isMute handle l = alloca $ \ptr -> do
  _ <- elem_gmute l handle 0 ptr
  val <- peek ptr
  return $ val == 0


getVolumeRange :: ElemHandle -> LibAlsa -> IO (Int, Int)
getVolumeRange handle l = alloca $ \lower -> alloca $ \upper -> do
  _ <- elem_gvrange l handle lower upper
  lowerv <- peek lower
  upperv <- peek upper
  return (fromIntegral lowerv, fromIntegral upperv)


getVolume :: ElemHandle -> LibAlsa -> IO Int
getVolume handle l = alloca $ \ptr -> do
  _ <- elem_gvol l handle 0 ptr
  val <- peek ptr
  return $ fromIntegral val


getMixerHandle :: String -> LibAlsa -> ExceptT Int IO MixerHandle
getMixerHandle card l = do
  handle <- openMixer l
  mixerAttach handle card l
  mixerRegister handle l
  mixerLoad handle l
  return handle


percentize :: Int -> Int -> Int -> Int
percentize val lower upper = 100 * (val - lower) `div` (upper-lower)


-- |The handle exported by this module
data VOLHandle = VOLH LibAlsa MixerHandle ElemHandle (IORef Int) (IORef Bool) Int Int | Err

{- |Update the volume handle.

This function has to be called to update the handle internally.
Calling this will get the current state into the handle, which can then by
queried by the other functions. Until this is called again, the results of other
functions will not update to the current state of the system.
-}
updateVOLH :: VOLHandle -> IO ()
updateVOLH (VOLH l handle ehandle valr muter _ _) = do
  mixer_handle_events l handle
  val <- getVolume ehandle l
  mute <- isMute ehandle l
  writeIORef valr val
  writeIORef muter mute
updateVOLH Err = return ()

-- |Get the raw volume value from alsa
getVolumeRaw :: VOLHandle -> IO Int
getVolumeRaw (VOLH _ _ _ valr _ _ _) = readIORef valr
getVolumeRaw Err = return 0

{- |Get the volume in percent (100% = loudest 0%=lowest)

0% does not equal a muted device.
-}
getVolumePercent :: VOLHandle -> IO Int
getVolumePercent (VOLH _ _ _ valr _ lower upper) = do
  val <- readIORef valr
  return $ percentize val lower upper
getVolumePercent Err = return 0

-- |return 'True' if the device is muted
getMute :: VOLHandle -> IO Bool
getMute (VOLH _ _ _ _ muter _ _) = readIORef muter
getMute Err = return True


getVOLHandleInt :: Either Int MixerHandle -> LibAlsa -> IO VOLHandle
getVOLHandleInt (Right handle) l = do
  ehandle <- getElem handle "Master" 0 l
  if ehandle == nullPtr
    then return Err
    else do
      (lower, upper) <- getVolumeRange ehandle l
      val <- getVolume ehandle l
      mute <- isMute ehandle l
      volref <- newIORef val
      muteref <- newIORef mute
      return (VOLH l handle ehandle volref muteref lower upper)
getVOLHandleInt _ _ = return Err

-- |Check if there was an error creating the handle
isLoaded :: VOLHandle -> Bool
isLoaded Err = False
isLoaded _ = True


-- |Get PollFds for polling interface
getPollFDs :: VOLHandle -> IO [Fd]
getPollFDs (VOLH l h _ _ _ _ _) = map Fd <$> getPollDescs h l
getPollFDs Err = return []

-- | Close the mixer handle and unload alsa library
destroyVOLHandle :: VOLHandle -> IO ()
destroyVOLHandle (VOLH a m _ _ _ _ _) =
  mixer_close a m >> destroyLibAlsa a
destroyVOLHandle Err = return ()

{- |Create an 'VOLHandle'

This function returns a type save error value if any alsa function fails
-}
getVOLHandle :: String -- ^The audio-card to use
             -> IO VOLHandle
getVOLHandle card = do
  l <- getLibAlsa
  handle <- runExceptT (getMixerHandle card l)
  getVOLHandleInt handle l
