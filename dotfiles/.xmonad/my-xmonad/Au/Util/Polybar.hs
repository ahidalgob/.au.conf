module Au.Util.Polybar
  ( MouseButton(..)
  , underline, overline, foreground, background
  , clickable
  )where

import Data.Monoid((<>))
import Text.Printf(printf)

data MouseButton
  = LeftClick         -- 1
  | MiddleClick       -- 2
  | RightClick        -- 3
  | ScrollUp          -- 4
  | ScrollDown        -- 5
  | DoubleLeftClick   -- 6
  | DoubleRightClick  -- 7
  | DoubleMiddleClick -- 8
  deriving Enum
buttonIndex :: MouseButton -> Int
buttonIndex bi = fromEnum bi + 1

-- lots of %
clickable :: MouseButton -> String -> String -> String
clickable b command s = printf "%%{A%d:%s:}%s%%{A}" (buttonIndex b) command s

underline, overline, foreground, background :: String -> String -> String
underline color s = printf "%%{u%s}%%{+u}%s%%{-u}" color s
overline color s = printf "%%{o%s}%%{+o}%s%%{-o}" color s
foreground color s = printf "%%{F%s}%s%%{F-}" color s
background color s = printf "%%{B%s}%s%%{B-}" color s
