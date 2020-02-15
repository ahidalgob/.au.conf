module Au.Colors where

import System.IO.Unsafe ( unsafePerformIO )
import System.Directory ( getHomeDirectory )
import System.FilePath.Posix ( (</>) )

-- this throws an error, probably an xmonad bug
--getXresourcesField :: String -> IO String
--getXresourcesField f = readProcess "/home/augusto/.au.conf/scripts/getXresourcesField" [f] ""

polybarBGAlpha :: String
polybarBGAlpha = snd ((read $ unsafePerformIO $ do
  home <- getHomeDirectory
  readFile $ home </> ".xmonad/my-xmonad/colorList") :: ([String], String))

allColors :: [String]
allColors = fst ((read $ unsafePerformIO $ do
  home <- getHomeDirectory
  readFile $ home </> ".xmonad/my-xmonad/colorList") :: ([String], String))

withAlpha :: String -> String -> String
withAlpha al col = "#" ++ al ++ tail col

color :: Int -> String
color c = allColors !! c
