module Au.Util.Screenshot(screenshot)
  where

import XMonad

screenshot :: Bool -> Bool -> X ()
screenshot sel cop = spawn $ "sleep 0.3; maim " ++ selec ++ copy ++
                      "~/Pictures/Screenshots/$(date +%s).png"
  where
    selec = if sel then "-s " else ""
    copy = if cop
              then "| xclip -selection clipboard -t image/png; " ++
                   "xclip -selection clipboard -o > "
              else ""

