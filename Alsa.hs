module Alsa 
(VOLHandle, getMute, getVolumeRaw, getVolumePercent, updateVOLH, getVOLHandle)
where


import Data.IORef
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

data RegOpt = RegOpt
data MClass = MClass

data Mixer = Mixer
type MixerHandle = Ptr Mixer
type MixerHandleAlloc = Ptr MixerHandle

data Sid = Sid
type SidHandle = Ptr Sid
type SidHandleAlloc = Ptr SidHandle

data Elem = Elem
type ElemHandle = Ptr Elem


foreign import ccall "snd_mixer_open" mixer_open :: MixerHandleAlloc -> Int -> IO CInt
foreign import ccall "snd_mixer_attach" mixer_attach :: MixerHandle -> CString -> IO CInt
foreign import ccall "snd_mixer_selem_register" mixer_register :: MixerHandle -> Ptr RegOpt -> Ptr MClass -> IO CInt
foreign import ccall "snd_mixer_load" mixer_load :: MixerHandle -> IO CInt

foreign import ccall "snd_mixer_selem_id_set_index" sid_sindex :: SidHandle -> CInt -> IO CInt
foreign import ccall "snd_mixer_selem_id_set_name" sid_sname :: SidHandle -> CString -> IO CInt
foreign import ccall "snd_mixer_selem_id_malloc" sid_alloc :: SidHandleAlloc -> IO CInt
foreign import ccall "snd_mixer_selem_id_free" sid_free :: SidHandle -> IO CInt

foreign import ccall "snd_mixer_find_selem" elem_find :: MixerHandle -> SidHandle -> IO ElemHandle
foreign import ccall "snd_mixer_selem_get_playback_volume_range" elem_gvrange :: ElemHandle -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "snd_mixer_selem_get_playback_volume" elem_gvol :: ElemHandle -> CInt -> Ptr CInt -> IO CInt
foreign import ccall "snd_mixer_selem_get_playback_switch" elem_gmute :: ElemHandle -> Int -> Ptr CInt -> IO CInt
foreign import ccall "snd_mixer_handle_events" mixer_handle_events :: MixerHandle -> IO ()



-- TODO fix error handling
openMixer :: IO MixerHandle
openMixer = alloca $ \ptr -> do
  ret <- mixer_open ptr 0
  peek ptr

mixerAttach :: MixerHandle -> String -> IO ()
mixerAttach handle card = do
  ret <- withCString card $ \ccard -> mixer_attach handle ccard
  return ()

mixerRegister :: MixerHandle -> IO ()
mixerRegister handle = do
  ret <- mixer_register handle nullPtr nullPtr
  return ()

mixerLoad :: MixerHandle -> IO ()
mixerLoad handle = do
  ret <- mixer_load handle
  return ()


withSid :: (SidHandle -> IO a) -> IO a
withSid fun = alloca $ \ptr -> do
  sid_alloc ptr
  handle <- peek ptr
  ret <- fun handle
  sid_free handle
  return ret


sidSet :: SidHandle -> Int -> String -> IO ()
sidSet handle index name = do
  ret <- withCString name $ \cname -> sid_sname handle cname
  ret2 <- sid_sindex handle $fromIntegral index
  return ()


getElem :: MixerHandle -> String -> Int -> IO ElemHandle
getElem handle name index = withSid $ \sid -> do
  sidSet sid index name
  elem_find handle sid

isMute :: ElemHandle -> IO Bool
isMute handle = alloca $ \ptr -> do
  elem_gmute handle 0 ptr
  val <- peek ptr
  return $val == 0


getVolumeRange :: ElemHandle -> IO (Int, Int)
getVolumeRange handle = alloca $ \min -> alloca $ \max -> do
  elem_gvrange handle min max
  minv <- peek min
  maxv <- peek max
  return (fromIntegral minv, fromIntegral maxv)


getVolume :: ElemHandle -> IO Int
getVolume handle = alloca $ \ptr -> do
  elem_gvol handle 0 ptr
  val <- peek ptr
  return $fromIntegral val


getMixerHandle :: String -> IO MixerHandle
getMixerHandle card = do
  handle <- openMixer
  mixerAttach handle card
  mixerRegister handle
  mixerLoad handle
  return handle

percentize :: Int -> Int -> Int -> Int
percentize val min max = 100 * (val - min) `div` (max-min)


getAudioPercent :: IO Int
getAudioPercent = do
  h <- getMixerHandle "default"
  e <- getElem h "Master" 0
  (min, max) <- getVolumeRange e
  val <- getVolume e
  return $percentize val min max


data VOLHandle = VOLH MixerHandle ElemHandle (IORef Int) (IORef Bool) Int Int

updateVOLH :: VOLHandle -> IO ()
updateVOLH (VOLH handle elem valr muter min max) = do
  mixer_handle_events handle
  val <- getVolume elem
  mute <- isMute elem
  writeIORef valr val
  writeIORef muter mute

getVolumeRaw :: VOLHandle -> IO Int
getVolumeRaw (VOLH _ _ valr _ _ _) = do
  val <- readIORef valr
  return val

getVolumePercent :: VOLHandle -> IO Int
getVolumePercent (VOLH _ _ valr _ min max) = do
  val <- readIORef valr
  return $percentize val min max

getMute :: VOLHandle -> IO Bool
getMute (VOLH _ _ _ muter _ _) = do
  mute <- readIORef muter
  return mute

getVOLHandle :: String -> IO VOLHandle
getVOLHandle card = do
  handle <- getMixerHandle card
  elem <- getElem handle "Master" 0
  (min, max) <- getVolumeRange elem
  val <- getVolume elem
  mute <- isMute elem
  volref <- newIORef val
  muteref <- newIORef mute
  return (VOLH handle elem volref muteref min max)
