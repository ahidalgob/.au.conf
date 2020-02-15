module Au.Workspaces where

import System.IO.Unsafe ( unsafePerformIO )
import System.Directory ( getHomeDirectory )
import System.FilePath.Posix ( (</>) )
import XMonad

myExtraWS = [("0", xK_0),("-", xK_minus),("=", xK_equal)]

myWorkspaces :: [String]
myWorkspaces = read (unsafePerformIO $ do
  home <- getHomeDirectory
  readFile $ home </> ".xmonad/my-xmonad/workspaces") ++ map fst myExtraWS
