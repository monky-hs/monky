{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-|
Module      : Alsa
Description : Allows acces to information about the alsa sound system
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides access to basic audio information provided by the alsa
audio system.
This MAY work with pulse, but will report useless/inaccurate values.
-}
module Monky.Alsa
(VOLHandle, getMute, getVolumeRaw, getVolumePercent, updateVOLH, getVOLHandle,
isLoaded, getPollFDs)
where

import Control.Applicative ((<$>))
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
import System.Posix.DynamicLinker
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


data LibAlsa = LibAlsa
  { mixer_open :: MixerHandleAlloc -> Int -> IO CInt
  , mixer_attach :: MixerHandle -> CString -> IO CInt
  , mixer_register :: MixerHandle -> Ptr RegOpt -> Ptr MClass -> IO CInt
  , mixer_load :: MixerHandle -> IO CInt

  , sid_sindex :: SidHandle -> CInt -> IO ()
  , sid_sname :: SidHandle -> CString -> IO ()
  , sid_alloc :: SidHandleAlloc -> IO CInt
  , sid_free :: SidHandle -> IO ()

  , elem_gvrange :: ElemHandle -> Ptr CInt -> Ptr CInt -> IO CInt
  , elem_gvol :: ElemHandle -> CInt -> Ptr CInt -> IO CInt
  , elem_gmute :: ElemHandle -> CInt -> Ptr CInt -> IO CInt

  , elem_find :: MixerHandle -> SidHandle -> IO ElemHandle
  , mixer_handle_events :: MixerHandle -> IO ()
  , get_pdescs :: MixerHandle -> PollFDPtr -> CInt -> IO CInt
  , get_pdescc :: MixerHandle -> IO CInt
  }

{-
foreign import ccall "snd_mixer_open" mixer_open :: MixerHandleAlloc -> Int -> IO CInt
foreign import ccall "snd_mixer_attach" mixer_attach :: MixerHandle -> CString -> IO CInt
foreign import ccall "snd_mixer_selem_register" mixer_register :: MixerHandle -> Ptr RegOpt -> Ptr MClass -> IO CInt
foreign import ccall "snd_mixer_load" mixer_load :: MixerHandle -> IO CInt

foreign import ccall "snd_mixer_selem_id_set_index" sid_sindex :: SidHandle -> CInt -> IO ()
foreign import ccall "snd_mixer_selem_id_set_name" sid_sname :: SidHandle -> CString -> IO ()
foreign import ccall "snd_mixer_selem_id_malloc" sid_alloc :: SidHandleAlloc -> IO CInt
foreign import ccall "snd_mixer_selem_id_free" sid_free :: SidHandle -> IO ()


foreign import ccall "snd_mixer_selem_get_playback_volume_range" elem_gvrange :: ElemHandle -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "snd_mixer_selem_get_playback_volume" elem_gvol :: ElemHandle -> CInt -> Ptr CInt -> IO CInt
foreign import ccall "snd_mixer_selem_get_playback_switch" elem_gmute :: ElemHandle -> CInt -> Ptr CInt -> IO CInt

foreign import ccall "snd_mixer_find_selem" elem_find :: MixerHandle -> SidHandle -> IO ElemHandle
foreign import ccall "snd_mixer_handle_events" mixer_handle_events :: MixerHandle -> IO ()
foreign import ccall "snd_mixer_poll_descriptors" get_pdescs :: MixerHandle -> PollFDPtr -> CInt -> IO CInt
foreign import ccall "snd_mixer_poll_descriptors_count" get_pdescc :: MixerHandle -> IO CInt
-}

{- dynamic linking -}
foreign import ccall "dynamic" mkFunmII :: FunPtr (MixerHandleAlloc -> Int -> IO CInt) -> (MixerHandleAlloc -> Int -> IO CInt)
foreign import ccall "dynamic" mkFunMSI :: FunPtr (MixerHandle -> CString -> IO CInt) -> (MixerHandle -> CString -> IO CInt)
foreign import ccall "dynamic" mkFunMrcI :: FunPtr (MixerHandle -> Ptr RegOpt -> Ptr MClass -> IO CInt) -> (MixerHandle -> Ptr RegOpt -> Ptr MClass -> IO CInt)

foreign import ccall "dynamic" mkFunSIV :: FunPtr (SidHandle -> CInt -> IO ()) -> (SidHandle -> CInt -> IO ())
foreign import ccall "dynamic" mkFunSSV :: FunPtr (SidHandle -> CString -> IO ()) -> (SidHandle -> CString -> IO ())
foreign import ccall "dynamic" mkFunsI  :: FunPtr (SidHandleAlloc -> IO CInt) -> (SidHandleAlloc -> IO CInt)
foreign import ccall "dynamic" mkFunSV  :: FunPtr (SidHandle -> IO ()) -> (SidHandle -> IO ())

foreign import ccall "dynamic" mkFunEiiI :: FunPtr (ElemHandle -> Ptr CInt -> Ptr CInt -> IO CInt) -> (ElemHandle -> Ptr CInt -> Ptr CInt -> IO CInt)
foreign import ccall "dynamic" mkFunEIiI :: FunPtr (ElemHandle -> CInt -> Ptr CInt -> IO CInt) -> (ElemHandle -> CInt -> Ptr CInt -> IO CInt)

foreign import ccall "dynamic" mkFunMSE :: FunPtr (MixerHandle -> SidHandle -> IO ElemHandle) -> (MixerHandle -> SidHandle -> IO ElemHandle)
foreign import ccall "dynamic" mkFunMV :: FunPtr (MixerHandle -> IO ()) -> (MixerHandle -> IO ())
foreign import ccall "dynamic" mkFunMI :: FunPtr (MixerHandle -> IO CInt) -> (MixerHandle -> IO CInt)
foreign import ccall "dynamic" mkFunMPII :: FunPtr (MixerHandle -> PollFDPtr -> CInt -> IO CInt) -> (MixerHandle -> PollFDPtr -> CInt -> IO CInt)



getPollDescs :: MixerHandle -> LibAlsa -> IO [CInt]
getPollDescs h l = do
  count <- get_pdescc l h
  allocaArray (fromIntegral count) $ \ptr -> do
    c2 <- get_pdescs l h ptr count
    if count == c2
      then liftM (map  (\(POLLFD fd _ _) -> fd)) (peekArray (fromIntegral c2) ptr)
      else return [] -- This should not happen


-- TODO fix error handling
openMixer :: LibAlsa -> ExceptT Int IO MixerHandle
openMixer l = liftExceptT alloca $ \ptr -> do
  rval <- liftIO (mixer_open l ptr 0)
  if rval < 0
     then throwE $fromIntegral rval
     else liftIO (peek ptr)

mixerAttach :: MixerHandle -> String -> LibAlsa -> ExceptT Int IO ()
mixerAttach handle card l = do
  rval <- liftIO(withCString card $ \ccard -> mixer_attach l handle ccard)
  if rval < 0
     then throwE $fromIntegral rval
     else liftIO (return ())

mixerRegister :: MixerHandle -> LibAlsa -> ExceptT Int IO ()
mixerRegister handle l = do
  rval <- liftIO(mixer_register l handle nullPtr nullPtr)
  if rval < 0
     then throwE $fromIntegral rval
     else liftIO(return ())

mixerLoad :: MixerHandle -> LibAlsa -> ExceptT Int IO ()
mixerLoad handle l = do
  rval <- liftIO(mixer_load l handle)
  if rval < 0
     then throwE $fromIntegral rval
     else liftIO(return ())


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
  withCString name $ \cname -> sid_sname l handle cname
  sid_sindex l handle $fromIntegral index


getElem :: MixerHandle -> String -> Int -> LibAlsa -> IO ElemHandle
getElem handle name index l = withSid l $ \sid -> do
  sidSet sid index name l
  elem_find l handle sid

isMute :: ElemHandle -> LibAlsa -> IO Bool
isMute handle l = alloca $ \ptr -> do
  _ <- elem_gmute l handle 0 ptr
  val <- peek ptr
  return $val == 0


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
  return $fromIntegral val


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
  return $percentize val lower upper
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
getPollFDs (VOLH l h _ _ _ _ _) = liftM (map (\x -> Fd x)) (getPollDescs h l)
getPollFDs Err =return []

getLib :: IO LibAlsa
getLib = do
  h <- dlopen "libasound.so" [RTLD_LAZY]
  mixer_open_ <- mkFunmII . castFunPtr <$> dlsym h "snd_mixer_open"
  mixer_attach_ <- mkFunMSI . castFunPtr <$> dlsym h "snd_mixer_attach"
  mixer_register_ <- mkFunMrcI . castFunPtr <$> dlsym h "snd_mixer_selem_register"
  mixer_load_ <- mkFunMI . castFunPtr <$> dlsym h "snd_mixer_load"

  sid_sindex_ <- mkFunSIV . castFunPtr <$> dlsym h "snd_mixer_selem_id_set_index"
  sid_sname_ <- mkFunSSV . castFunPtr <$> dlsym h "snd_mixer_selem_id_set_name"
  sid_alloc_ <- mkFunsI . castFunPtr <$> dlsym h "snd_mixer_selem_id_malloc"
  sid_free_ <- mkFunSV . castFunPtr <$> dlsym h "snd_mixer_selem_id_free"

  elem_gvrange_ <- mkFunEiiI . castFunPtr <$> dlsym h "snd_mixer_selem_get_playback_volume_range"
  elem_gvol_ <- mkFunEIiI . castFunPtr <$> dlsym h "snd_mixer_selem_get_playback_volume"
  elem_gmute_ <- mkFunEIiI . castFunPtr <$> dlsym h "snd_mixer_selem_get_playback_switch"

  elem_find_ <- mkFunMSE . castFunPtr <$> dlsym h "snd_mixer_find_selem"
  mixer_handle_events_ <- mkFunMV . castFunPtr <$> dlsym h "snd_mixer_handle_events"
  get_pdescs_ <- mkFunMPII . castFunPtr <$> dlsym h "snd_mixer_poll_descriptors"
  get_pdescc_ <- mkFunMI . castFunPtr <$> dlsym h "snd_mixer_poll_descriptors_count"
  return $LibAlsa mixer_open_ mixer_attach_ mixer_register_ mixer_load_ sid_sindex_ sid_sname_ sid_alloc_ sid_free_ elem_gvrange_ elem_gvol_ elem_gmute_ elem_find_ mixer_handle_events_ get_pdescs_ get_pdescc_

{- |Create an 'VOLHandle'

This may throw an exception if a function provided by alsa fails.
-}
getVOLHandle :: String -- ^The audio-card to use
             -> IO VOLHandle
getVOLHandle card = do
  l <- getLib
  handle <- runExceptT (getMixerHandle card l)
  getVOLHandleInt handle l
