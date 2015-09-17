{-# LANGUAGE ExistentialQuantification #-}
module Modules
-- We cannot limit export here because of the instances (move them?)
where

import System.Posix.Types
import Text.Printf (printf)

import Config
import Power
import Battery
import CPU
import Disk
import Memory
import Network
import Time
import Utility
import SSID
import Alsa

data Modules = forall a . Module a => MW a
class Module a where
    getText :: String -> a -> IO String
    getInterval :: a -> Int
    getFDs :: a -> IO [Fd]
    getFDs _ = return []

{- CPU Module -}

cpuColor :: Int -> String
cpuColor p
  | p < 15 = "#009900"
  | p < 50 = "#ffff66"
  | p < 90 = "#ff6600"
  | otherwise = "#ff0000"

formatCPUText :: String -> [Int] -> Int -> Float -> String
formatCPUText user cp ct cf =
  let bars = map printbars cp :: [String] in
  (freq ++ concat bars ++ printf " %d°C" ct)
  where 
    printbars pc = printf "^p(3)^pa(;0)^bg(%s)^r(6x8)^p(-6)^fg(#222222)^r(6x%d)^bg()^pa()^fg()" (cpuColor pc) (16- div (16 * pc) 100) :: String
    freq = printf ("^i(/home/" ++ user ++ "/.xmonad/xbm/cpu.xbm) %.1fG ^p(-3)") cf :: String

getCPUText :: String -> CPUHandle -> IO String
getCPUText user ch = do
  cp <- getCPUPercent ch
  ct <- getCPUTemp ch
  cf <- getCPUMaxScalingFreq ch
  return (formatCPUText user cp ct cf)

instance Module CPUHandle where
  getText = getCPUText
  getInterval _ = 5


{- Battery Module -}

batteryColor :: Int -> Int -> String
batteryColor s p
  | s == 1 = "#009900"
  | p < 20 = "#ffaf00"
  | p < 15 = "#ff8700"
  | p < 10 = "#ff5f00"
  | p <  5 = "#ff0000"
  | otherwise = ""

batterySymbol :: Int -> Int -> String -> String
batterySymbol s p user
  | s == 1 = "/home/" ++ user ++ "/.xmonad/xbm/ac_01.xbm"
  | p < 50 = "/home/" ++ user ++ "/.xmonad/xbm/bat_low_01.xbm"
  | p < 20 = "/home/" ++ user ++ "/.xmonad/xbm/bat_empty_01.xbm"
  | otherwise = "/home/" ++ user ++ "/.xmonad/xbm/bat_full_01.xbm"

formatBatteryText :: String -> Int -> Int -> Int -> Float -> String
formatBatteryText user p s online pow =
  printf "^fg(%s)^i(%s) %.1fW %3d%% %2d:%02d^fg()" (batteryColor online p) (batterySymbol online p user) pow p h m :: String
  where 
    h = s `div` 3600
    m = (s - h * 3600) `div` 60

getBatteryText :: String -> BatteryHandle -> IO String
getBatteryText user bh = do
  p <- getCurrentLevel bh
  s <- getTimeLeft bh
  online <- getCurrentStatus bh
  pow <- getLoading bh
  return (formatBatteryText user p s online pow)

instance Module BatteryHandle where
  getText = getBatteryText
  getInterval _ = 5


{- Network Module -}

formatNetworkText :: Maybe (Int, Int) -> String
formatNetworkText Nothing =
  printf "Network: off" :: String
formatNetworkText (Just (r, w)) =
  printf "%s %s" (convertUnit r  "B" "k" "M" "G") (convertUnit w "B" "k" "M" "G") :: String

getNetworkText :: String -> NetworkHandles -> IO String
getNetworkText _ nh = do
  nv <- getReadWriteMulti nh
  return (formatNetworkText nv)


instance Module NetworkHandles where
  getText = getNetworkText
  getInterval _ = 5

{- Memory Module -}

getMemoryText :: String -> MemoryHandle -> IO String
getMemoryText user mh = do
  mp <- getMemoryAvailable mh
  return (printf ("^i(/home/" ++ user ++ "/.xmonad/xbm/mem.xbm) %s") (convertUnit mp "B" "K" "M" "G") :: String)

instance Module MemoryHandle where
  getText = getMemoryText
  getInterval _ = 5


{- Time Module -}

timeToXBM :: (Int, Int) -> (Int, Int)
timeToXBM (h, m) = (xh, xm)
  where xh = h `mod` 12
        xm = m `div` 15

getTimeString :: String -> TimeHandle -> IO String
getTimeString user h = do
  ts <- getTime (getFormat h)
  t <- getHM
  let (th, tm) = timeToXBM t
  return (printf ("^i(/home/" ++ user ++ "/.xmonad/xbm/%d-%d.xbm)  %s") th tm ts)

instance Module TimeHandle where
    getText = getTimeString
    getInterval _ = 1

{- Disk module -}

formatDiskText :: String -> Int -> Int -> Int -> String
formatDiskText user dr dw df =
  printf "%s %s" eins zwei :: String
  where
    eins = printf ("^i(/home/" ++ user ++ "/.xmonad/xbm/diskette.xbm) %dG") df :: String
    zwei = printf "%s %s" (convertUnit dr  "B" "k" "M" "G") (convertUnit dw "B" "k" "M" "G") :: String

getDiskText :: String -> DiskHandle -> IO String
getDiskText u dh = do
  (dr, dw) <- getDiskReadWrite dh
  df <- getDiskFree dh
  return (formatDiskText u dr dw df)

instance Module DiskHandle where
  getText = getDiskText
  getInterval _ = 5

{- SSID module -}

instance Module SSIDHandle where
  getText _ = getCurrentSSID
  getInterval _ = 10

{- ALSA module -}

getVolumeStr :: VOLHandle -> IO String
getVolumeStr h = do
  updateVOLH h
  m <- getMute h
  v <- getVolumePercent h
  if m
    then return "Mute"
    else return $printf "% 3d%%" v

instance Module VOLHandle where
  getText _ = getVolumeStr
  getInterval _ = 0
  getFDs = getPollFDs


{- Module list -}
getModules :: IO [Modules]
getModules = do
  sh <- getSSIDHandle wifi_device
  vh <- getVOLHandle "default"
  nh <- getNetworkHandles network_devices
  ch <- getCPUHandle ScalingCur
  mh <- getMemoryHandle
  ph <- getPowerHandle
  bh <- getBatteryHandle ph
  --dh <- getDiskHandle disk_drive disk_part
  let th = getTimeHandle "%m/%d %k:%M:%S"
  return
    [ MW sh
    , MW vh
    , MW ch
    , MW nh
    , MW mh
    , MW bh
    --, MW dh
    , MW th
    ]