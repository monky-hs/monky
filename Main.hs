
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

cpuColor :: Int -> String
cpuColor p
  | p < 15 = "#009900"
  | p < 50 = "#ffff66"
  | p < 90 = "#ff6600"
  | otherwise = "#ff0000"

batterySymbol :: Int -> Int -> String -> String
batterySymbol s p user
  | s == 1 = "/home/" ++ user ++ "/.xmonad/xbm/ac_01.xbm"
  | p < 50 = "/home/" ++ user ++ "/.xmonad/xbm/bat_low_01.xbm"
  | p < 20 = "/home/" ++ user ++ "/.xmonad/xbm/bat_empty_01.xbm"
  | otherwise = "/home/" ++ user ++ "/.xmonad/xbm/bat_full_01.xbm"

printNetwork :: Maybe (Int, Int) -> IO()
printNetwork Nothing = do
  printf " |"
  printf "Network: off"
printNetwork (Just (r, w)) = do
  printf " |"
  printf " %s %s" (convertUnit r  "B" "k" "M" "G") (convertUnit w "B" "k" "M" "G")

printCPU :: String -> [Int] -> Int -> Float -> IO ()
printCPU user cp ct cf = do
  printf ("^i(/home/" ++ user ++ "/.xmonad/xbm/cpu.xbm) %.1fG ^p(-3)") cf
  mapM_ printbars cp
  printf " %d°C" ct
  where printbars pc = (printf "^p(3)^pa(;0)^bg(%s)^r(6x8)^p(-6)^fg(#222222)^r(6x%d)^bg()^pa()^fg()") (cpuColor pc) (16- (flip div 100  (16 * pc)))

timeToXBM :: (Int, Int) -> (Int, Int)
timeToXBM (h, m) = (xh, xm)
  where xh = h `mod` 12
        xm = m `div` 15

mainLoop :: String -> BatteryHandle -> NetworkHandles -> CPUHandle -> MemoryHandle -> PowerHandle -> DiskHandle ->  IO()
mainLoop user bh nh ch mh ph dh = do
  p <- getCurrentLevel bh
  s <- getTimeLeft bh
  online <- getCurrentStatus bh
  pow <- getPowerNow ph
  let h = s `div` 3600
  let m = (s - h * 3600) `div` 60
  nv <- getReadWriteMulti nh
  ts <- getTime "%a %m/%d %k:%M:%S"
  t <- getHM
  let (th, tm) = timeToXBM t
  cp <- getCPUPercent ch
  ct <- getCPUTemp ch
  cf <- getCPUMaxScalingFreq ch
  mp <- getMemoryAvailable mh
  (dr, dw) <- getDiskReadWrite dh
  df <- getDiskFree dh
--  putStrLn (printf "%.1fW %3d%% %02d:%02d | MemFree: %dM | R/W: %dkbit/%dkbit | %s" pow p h m mp (r`div`1000`div`125) (w`div`1000`div`125) ts)
--  putStrLn ((show cp) ++ (show ct))
--  printf "%.1f\n" cf
--  printf "%d %d\n" dr dw
--  printf "^C^i(/home/ongy/.xmonad/xbm/cpu.xbm) %.1fG\n" cf

-- format CPU section
  printCPU user cp ct cf
-- format Network section
  printNetwork nv
-- format Disk section
  printf " |"
  printf (" ^i(/home/" ++ user ++ "/.xmonad/xbm/diskette.xbm) %dG") df
  printf " %s %s" (convertUnit dr  "B" "k" "M" "G") (convertUnit dw "B" "k" "M" "G")
-- format Memory section
  printf " |"
  printf (" ^i(/home/" ++ user ++ "/.xmonad/xbm/mem.xbm) %s") (convertUnit mp "B" "K" "M" "G")
-- format Battery section
  printf " |"
  printf " ^fg(%s)^i(%s) %.1fW %3d%% %2d:%02d^fg()" (batteryColor online p) (batterySymbol online p user) pow p h m
-- format Time section
  printf " |"
  printf (" ^i(/home/" ++ user ++ "/.xmonad/xbm/%d-%d.xbm)  %s") th tm ts
  printf "\n"
  hFlush stdout
  threadDelay 1000000
  mainLoop user bh nh ch mh ph dh

main :: IO()
main = do
  bh <- getBatteryHandle
  nh <- getNetworkHandles network_devices
  ch <- getCPUHandle
  mh <- getMemoryHandle
  ph <- getPowerHandle
  dh <- getDiskHandle disk_drive disk_part
  user <- getEffectiveUserName
  mainLoop user bh nh ch mh ph dh
