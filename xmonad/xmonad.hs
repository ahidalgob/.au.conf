import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers -- Full Screen
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W

curLayout :: X String
curLayout = gets windowset >>= return . description . W.layout . W.workspace . W.current

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ defaultConfig
        { terminal           = "urxvt -e tmux"
        , borderWidth        = 3
        , normalBorderColor  = "#505050"
        , focusedBorderColor = "#6c99bb"

        , manageHook = manageDocks <+> (isFullscreen --> doFullFloat) <+> manageHook defaultConfig
        , layoutHook = smartBorders $ avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }

        --, modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((mod1Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock; xset dpms force off")
        , ((mod1Mask, xK_a     ), sendMessage NextLayout >> (curLayout >>= \d->spawn $"xmessage "++d))
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((mod1Mask, xK_f), withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1))
        ]
