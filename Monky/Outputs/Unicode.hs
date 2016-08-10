{-
    Copyright 2016 Markus Ongyerth

    This file is part of Monky.

    Monky is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Monky is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Monky.  If not, see <http://www.gnu.org/licenses/>.
-}
{-|
Module      : Monky.Outputs.Unicode
Description : Provides helper functions for output generators that use unicode
Maintainer  : ongy
Stability   : testing
Portability : Linux

-}
module Monky.Outputs.Unicode
where

-- |Unicode block char for vertical bar (in %)
barChar :: Integral a => a -> Char
barChar i
  | i <= 0                = ' '
  | i < (100 `div` 8)     = '▁'
  | i < (100 `div` 4)     = '▂'
  | i < (100 `div` 8 * 3) = '▃'
  | i < (100 `div` 2)     = '▄'
  | i < (100 `div` 8 * 5) = '▅'
  | i < (100 `div` 4 * 3) = '▆'
  | i < (100 `div` 8 * 7) = '▇'
  | otherwise             = '█'

-- |Horizontalblock char for horizontal bar (in % of char)
hBarChar :: Integral a => a -> Char
hBarChar i
  | i < (100 `div` 8)     = '▏'
  | i < (100 `div` 4)     = '▎'
  | i < (100 `div` 8 * 3) = '▍'
  | i < (100 `div` 2)     = '▌'
  | i < (100 `div` 8 * 5) = '▋'
  | i < (100 `div` 4 * 3) = '▊'
  | i < (100 `div` 8 * 7) = '▉'
  | otherwise             = '█'

