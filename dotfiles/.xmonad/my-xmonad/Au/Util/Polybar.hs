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

ifNotEmpty :: (String -> String) -> (String -> String)
ifNotEmpty f s = if s == "" then "" else f s

-- lots of %
clickable :: MouseButton -> String -> String -> String
clickable b command = ifNotEmpty $
  printf "%%{A%d:%s:}%s%%{A}" (buttonIndex b) command

underline, overline, foreground, background :: String -> String -> String
underline color = ifNotEmpty $ printf "%%{u%s}%%{+u}%s%%{-u}" color
overline color = ifNotEmpty $ printf "%%{o%s}%%{+o}%s%%{-o}" color
foreground color = ifNotEmpty $ printf "%%{F%s}%s%%{F-}" color
background color = ifNotEmpty $ printf "%%{B%s}%s%%{B-}" color
