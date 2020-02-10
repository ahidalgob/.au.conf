module Au.Colors where

import System.IO.Unsafe ( unsafePerformIO )
import System.Directory ( getHomeDirectory )
import System.FilePath.Posix ( (</>) )

-- this throws an error, probably an xmonad bug
--getXresourcesField :: String -> IO String
--getXresourcesField f = readProcess "/home/augusto/.au.conf/scripts/getXresourcesField" [f] ""

withAlpha :: String -> String -> String
withAlpha al col = "#" ++ al ++ tail col

allColors :: [String]
allColors = read $ unsafePerformIO $ do
  home <- getHomeDirectory
  readFile $ home </> ".xmonad/my-xmonad/colorList"

color :: Int -> String
color c = allColors !! c
