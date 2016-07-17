{-|
Module      : Monky.Examples.TimeFancy
Description : A slightelly more fancy example module instance for the time module
Maintainer  : ongy, moepi
Stability   : testing
Portability : Linux

-}
module Monky.Examples.TimeFancy ()
where

import qualified Data.Text as T
import Monky.Modules
import Monky.Time

{- Time Module -}
timeToXBM :: (Int, Int) -> (Int, Int)
timeToXBM (h, m) = (xh, xm)
  where xh = h `mod` 12
        xm = m `div` 15

getTimeString :: String -> TimeHandle -> IO String
getTimeString user h = do
  ts <- getTime h
  t <- getHM h
  let (th, tm) = timeToXBM t
  return ("^i(/home/" ++ user ++ "/.monky/xbm/" ++ show th ++ ('-': show tm) ++ ".xbm)  " ++ ts)

-- |Example instance for time module
instance Module TimeHandle where
    getText = getTimeString

instance NewModule TimeHandle where
  getOutput h = do
    ts <- getTime h
    t <- getHM h
    let (th, tm) = timeToXBM t
    return
      [ MonkyImage . T.pack $ (show th ++ '-':show tm ++ ".xbm")
      , MonkyPlain . T.pack $ ts
      ]
