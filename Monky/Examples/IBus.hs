{-# LANGUAGE CPP #-}
module Monky.Examples.IBus
  ( getIBusH
  )
where


#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif


import Monky.Modules
import System.Posix.IO
import System.Posix.Types (Fd)

import IBus
import IBus.EngineDesc


data IBusH = IBusH IBusClient [(String, String)]

instance Module IBusH where
  getText = getText'
  getFDs = getFD
  getEventText = getEventText'

getIBusH :: [(String, String)] -> IO IBusH
getIBusH m = fmap (\h -> IBusH h m) iBusConnect 

getFD :: IBusH -> IO [Fd]
getFD (IBusH h _) = do
  (r, w) <- createPipe
  _ <- subscribeToEngine
    h
    (\xs -> fdWrite w (head xs ++ "\n") >> return ())
  return [r]

remapEngine :: [(String, String)] -> String -> String
remapEngine [] x = x
remapEngine ((l,r):xs) x = if l == x
  then r
  else remapEngine xs x

getText' :: String -> IBusH -> IO String
getText' _ (IBusH h m) = do
  engine <- engineName <$> getIBusEngine h
  return $remapEngine m engine

getEventText' :: Fd -> String -> IBusH -> IO String
getEventText' fd _ (IBusH _ m) = do
-- The 'last . lines' part ensures we get the last event
-- If we didn't do this a rapid change in engines 
-- (two update events before we read once) could do weird stuff
  engine <- last . lines . fst <$> fdRead fd 512
  return $remapEngine m engine

