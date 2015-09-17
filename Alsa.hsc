{-|
Module      : Alsa
Description : Allows acces to information about the alsa sound system
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module interfaces with libalsa directly over ffi and a bunch of code
written as hsc.
-}
module Alsa
(VOLHandle, getMute, getVolumeRaw, getVolumePercent, updateVOLH, getVOLHandle,
isLoaded, getPollFDs)
where

import Control.Monad (liftM)
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.IORef
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Types

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


foreign import ccall "snd_mixer_open" mixer_open :: MixerHandleAlloc -> Int -> IO CInt
foreign import ccall "snd_mixer_attach" mixer_attach :: MixerHandle -> CString -> IO CInt
foreign import ccall "snd_mixer_selem_register" mixer_register :: MixerHandle -> Ptr RegOpt -> Ptr MClass -> IO CInt
foreign import ccall "snd_mixer_load" mixer_load :: MixerHandle -> IO CInt

foreign import ccall "snd_mixer_selem_id_set_index" sid_sindex :: SidHandle -> CInt -> IO ()
foreign import ccall "snd_mixer_selem_id_set_name" sid_sname :: SidHandle -> CString -> IO ()
foreign import ccall "snd_mixer_selem_id_malloc" sid_alloc :: SidHandleAlloc -> IO CInt
foreign import ccall "snd_mixer_selem_id_free" sid_free :: SidHandle -> IO ()

foreign import ccall "snd_mixer_find_selem" elem_find :: MixerHandle -> SidHandle -> IO ElemHandle
foreign import ccall "snd_mixer_selem_get_playback_volume_range" elem_gvrange :: ElemHandle -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "snd_mixer_selem_get_playback_volume" elem_gvol :: ElemHandle -> CInt -> Ptr CInt -> IO CInt
foreign import ccall "snd_mixer_selem_get_playback_switch" elem_gmute :: ElemHandle -> Int -> Ptr CInt -> IO CInt
foreign import ccall "snd_mixer_handle_events" mixer_handle_events :: MixerHandle -> IO ()

foreign import ccall "snd_mixer_poll_descriptors" get_pdescs :: MixerHandle -> PollFDPtr -> CInt -> IO CInt
foreign import ccall "snd_mixer_poll_descriptors_count" get_pdescc :: MixerHandle -> IO CInt


getPollDescs :: MixerHandle -> IO [CInt]
getPollDescs h = do
  count <- get_pdescc h
  allocaArray (fromIntegral count) $ \ptr -> do
    c2 <- get_pdescs h ptr count
    if count == c2
      then liftM (map  (\(POLLFD fd _ _) -> fd)) (peekArray (fromIntegral c2) ptr)
      else return [] -- This should not happen


-- TODO fix error handling
openMixer :: ExceptT Int IO MixerHandle
openMixer = liftExceptT alloca $ \ptr -> do
  rval <- liftIO (mixer_open ptr 0)
  if rval < 0
     then throwE $fromIntegral rval
     else liftIO (peek ptr)

mixerAttach :: MixerHandle -> String -> ExceptT Int IO ()
mixerAttach handle card = do
  rval <- liftIO(withCString card $ \ccard -> mixer_attach handle ccard)
  if rval < 0
     then throwE $fromIntegral rval
     else liftIO (return ())

mixerRegister :: MixerHandle -> ExceptT Int IO ()
mixerRegister handle = do
  rval <- liftIO(mixer_register handle nullPtr nullPtr)
  if rval < 0
     then throwE $fromIntegral rval
     else liftIO(return ())

mixerLoad :: MixerHandle -> ExceptT Int IO ()
mixerLoad handle = do
  rval <- liftIO(mixer_load handle)
  if rval < 0
     then throwE $fromIntegral rval
     else liftIO(return ())


withSid :: (SidHandle -> IO a) -> IO a
withSid fun = alloca $ \ptr -> do
  rval <- sid_alloc ptr
  if rval < 0
    then error "Failed to allocate sid"
    else do
      handle <- peek ptr
      comp <- fun handle
      sid_free handle
      return comp


sidSet :: SidHandle -> Int -> String -> IO ()
sidSet handle index name = do
  withCString name $ \cname -> sid_sname handle cname
  sid_sindex handle $fromIntegral index


getElem :: MixerHandle -> String -> Int -> IO ElemHandle
getElem handle name index = withSid $ \sid -> do
  sidSet sid index name
  elem_find handle sid

isMute :: ElemHandle -> IO Bool
isMute handle = alloca $ \ptr -> do
  _ <- elem_gmute handle 0 ptr
  val <- peek ptr
  return $val == 0


getVolumeRange :: ElemHandle -> IO (Int, Int)
getVolumeRange handle = alloca $ \lower -> alloca $ \upper -> do
  _ <- elem_gvrange handle lower upper
  lowerv <- peek lower
  upperv <- peek upper
  return (fromIntegral lowerv, fromIntegral upperv)


getVolume :: ElemHandle -> IO Int
getVolume handle = alloca $ \ptr -> do
  _ <- elem_gvol handle 0 ptr
  val <- peek ptr
  return $fromIntegral val


getMixerHandle :: String -> ExceptT Int IO MixerHandle
getMixerHandle card = do
  handle <- openMixer
  mixerAttach handle card
  mixerRegister handle
  mixerLoad handle
  return handle

percentize :: Int -> Int -> Int -> Int
percentize val lower upper = 100 * (val - lower) `div` (upper-lower)



-- |The handle exported by this module
data VOLHandle = VOLH MixerHandle ElemHandle (IORef Int) (IORef Bool) Int Int | Err

-- |Update the volume handle, this is a alsa specific function
updateVOLH :: VOLHandle -> IO ()
updateVOLH (VOLH handle ehandle valr muter _ _) = do
  mixer_handle_events handle
  val <- getVolume ehandle
  mute <- isMute ehandle
  writeIORef valr val
  writeIORef muter mute
updateVOLH Err = return ()

-- |Get the raw volume value from alsa
getVolumeRaw :: VOLHandle -> IO Int
getVolumeRaw (VOLH _ _ valr _ _ _) = readIORef valr
getVolumeRaw Err = return 0

{- |Get the volume in percent (100% = loudest 0%=lowest)

0% does not equal a muted device
-}
getVolumePercent :: VOLHandle -> IO Int
getVolumePercent (VOLH _ _ valr _ lower upper) = do
  val <- readIORef valr
  return $percentize val lower upper
getVolumePercent Err = return 0

-- |return 'True' if the device is muted
getMute :: VOLHandle -> IO Bool
getMute (VOLH _ _ _ muter _ _) = readIORef muter
getMute Err = return True

getVOLHandleInt :: Either Int MixerHandle -> IO VOLHandle
getVOLHandleInt (Right handle) = do
  ehandle <- getElem handle "Master" 0
  if ehandle == nullPtr
    then return Err
    else do
      (lower, upper) <- getVolumeRange ehandle
      val <- getVolume ehandle
      mute <- isMute ehandle
      volref <- newIORef val
      muteref <- newIORef mute
      return (VOLH handle ehandle volref muteref lower upper)
getVOLHandleInt _ = return Err

-- |Check if there was an error creating the handle
isLoaded :: VOLHandle -> Bool
isLoaded Err = False
isLoaded _ = True


-- |Get PollFds for polling interface
getPollFDs :: VOLHandle -> IO [Fd]
getPollFDs (VOLH h _ _ _ _ _) = liftM (map (\x -> Fd x)) (getPollDescs h)
getPollFDs Err =return []

{- |Get the alsa handle

This calls quite a few functions form libalsa and may throw an error if
something happened.
-}
getVOLHandle :: String -> IO VOLHandle
getVOLHandle card = do
  handle <- runExceptT (getMixerHandle card)
  getVOLHandleInt handle
