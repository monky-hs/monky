
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

batteryColor :: Int -> String
batteryColor p
  | p < 20 = "#ffaf00"
  | p < 15 = "#ff8700"
  | p < 10 = "#ff5f00"
  | p <  5 = "#ff0000"
  | otherwise = ""

batterySymbol :: Int -> Int -> String
batterySymbol s p
  | s == 1 = "/home/moepi/.xmonad/xbm/ac_01.xbm"
  | p < 50 = "/home/moepi/.xmonad/xbm/bat_low_01.xbm"
  | p < 20 = "/home/moepi/.xmonad/xbm/bat_empty_01.xbm"
  | otherwise = "/home/moepi/.xmonad/xbm/bat_full_01.xbm"

mainLoop :: BatteryHandle -> NetworkHandle -> CPUHandle -> MemoryHandle -> PowerHandle -> DiskHandle -> IO()
mainLoop bh nh ch mh ph dh = do
  (p, bc) <- getCurrentLevel bh
  (s, bn) <- getTimeLeft bc
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
--  printf "^C^i(/home/moepi/.xmonad/xbm/cpu.xbm) %.1fG\n" cf

-- format CPU section
  printf "^i(/home/moepi/.xmonad/xbm/cpu.xbm) %.1fG ^p(-3)" cf
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
  printf " ^i(/home/moepi/.xmonad/xbm/mem.xbm) %s" (convertUnit mp "B" "K" "M" "G")
-- format Battery section
  printf " |"
  printf " ^fg(%s)^i(%s) %.1fW %3d%% %2d:%02d^fg()" (batteryColor p) (batterySymbol 0 p) pow p h m
-- format Time section
  printf " |"
  printf " ^i(/home/moepi/.xmonad/xbm/clock.xbm)  %s" ts
  printf "\n"
  hFlush stdout
  threadDelay 1000000
  mainLoop bn nn cn mn ph dn

main :: IO()
main = do
  bh <- getBatteryHandle
  nh <- getNetworkHandle "wlan0"
  ch <- getCPUHandle
  mh <- getMemoryHandle
  ph <- getPowerHandle
  dh <- getDiskHandle "sda" 3
  mainLoop bh nh ch mh ph dh
