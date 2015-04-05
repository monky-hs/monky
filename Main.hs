
import Battery
import Network
import Time
import CPU
import Memory
import Power
import Control.Concurrent (threadDelay)
import Text.Printf (printf)

mainLoop :: BatteryHandle -> NetworkHandle -> CPUHandle -> MemoryHandle -> PowerHandle -> IO()
mainLoop bh nh ch mh ph = do
  (p, bc) <- getCurrentLevel bh
  (s, bn) <- getTimeLeft bc
  (pow, ph) <- getPowerNow ph
  let h = s `div` 3600
  let m = (s - h * 3600) `div` 60
  (r, w, nn) <- getReadWrite nh
  ts <- getTime "%H:%M %d.%m.%y"
  (cp, cn) <- getCPUPercent ch
  (ct, cn) <- getCPUTemp cn
  (cf, cn) <- getCPUMaxScalingFreq cn
  (mp, mn) <- getMemoryAvailable mh
  putStrLn (printf "%.1fW %3d%% %02d:%02d | MemFree: %dM | R/W: %dMbit/%dMbit | %s" pow p h m mp (r`div`1000`div`125) (w`div`1000`div`125) ts)
  putStrLn ((show cp) ++ (show ct))
  printf "%.1f\n" cf
  threadDelay 1000000
  mainLoop bn nn cn mn ph

main :: IO()
main = do
  bh <- getBatteryHandle
  nh <- getNetworkHandle "wlan0"
  ch <- getCPUHandle
  mh <- getMemoryHandle
  ph <- getPowerHandle
  mainLoop bh nh ch mh ph
