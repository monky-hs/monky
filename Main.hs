
import Battery
import Network
import Time
import CPU
import Memory
import Power
import Disk
import Utility
import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import System.IO
import Data.Array.Storable
import Data.Array.Unsafe
import qualified Data.Text
import Config
import System.Posix.User (getEffectiveUserName)

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

mainLoop :: String -> BatteryHandle -> NetworkHandle -> CPUHandle -> MemoryHandle -> PowerHandle -> DiskHandle ->  IO()
mainLoop user bh nh ch mh ph dh = do
  (p, bh) <- getCurrentLevel bh
  (s, bh) <- getTimeLeft bh
  (online, bh) <- getCurrentStatus bh
  (pow, ph) <- getPowerNow ph
  let h = s `div` 3600
  let m = (s - h * 3600) `div` 60
  (r, w, nn) <- getReadWrite nh
  ts <- getTime "%a %m/%d %k:%M:%S"
  (cp, cn) <- getCPUPercent ch
  (ct, cn) <- getCPUTemp cn
  (cf, cn) <- getCPUMaxScalingFreq cn
  (mp, mn) <- getMemoryAvailable mh
  (dr, dw, dn) <- getDiskReadWrite dh
  (df, dn) <- getDiskFree dn
--  putStrLn (printf "%.1fW %3d%% %02d:%02d | MemFree: %dM | R/W: %dkbit/%dkbit | %s" pow p h m mp (r`div`1000`div`125) (w`div`1000`div`125) ts)
--  putStrLn ((show cp) ++ (show ct))
--  printf "%.1f\n" cf
--  printf "%d %d\n" dr dw
--  printf "^C^i(/home/ongy/.xmonad/xbm/cpu.xbm) %.1fG\n" cf

-- format CPU section
  printf ("^i(/home/" ++ user ++ "/.xmonad/xbm/cpu.xbm) %.1fG ^p(-3)") cf
  mapM_ (printf "^p(3)^pa(;0)^bg(#777777)^r(6x8)^p(-6)^fg(#222222)^r(6x%d)^bg()^pa()^fg()") (map (16-) $ map (`div`100) $ map (*16) cp)
  printf " %d°C" ct
-- format Network section
  printf " |"
  printf " %s %s" (convertUnit r  "B" "k" "M" "G") (convertUnit w "B" "k" "M" "G")
-- format Disk section
  printf " |"
  printf " ^i(/home/moepi/.xmonad/xbm/diskette.xbm) %dG" df
  printf " %s %s" (convertUnit dr  "B" "k" "M" "G") (convertUnit dw "B" "k" "M" "G")
-- format Memory section
  printf " |"
  printf (" ^i(/home/" ++ user ++ "/.xmonad/xbm/mem.xbm) %s") (convertUnit mp "B" "K" "M" "G")
-- format Battery section
  printf " |"
  printf " ^fg(%s)^i(%s) %.1fW %3d%% %2d:%02d^fg()" (batteryColor online p) (batterySymbol online p user) pow p h m
-- format Time section
  printf " |"
  printf (" ^i(/home/" ++ user ++ "/.xmonad/xbm/clock.xbm)  %s") ts
  printf "\n"
  hFlush stdout
  threadDelay 1000000
  mainLoop user bh nn cn mn ph dn

main :: IO()
main = do
  bh <- getBatteryHandle
  nh <- getNetworkHandle network_device
  ch <- getCPUHandle
  mh <- getMemoryHandle
  ph <- getPowerHandle
  dh <- getDiskHandle "sda" 3
  user <- getEffectiveUserName
  mainLoop user bh nh ch mh ph dh
