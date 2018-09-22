import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Script
import XMonad.Hooks.SetWMName
import XMonad.Hooks.FadeInactive
import XMonad.Layout.Circle
import XMonad.Layout.Spiral
import XMonad.Layout.Spacing
import XMonad.Layout.Fullscreen
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders

import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers
import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.Xinerama
import Control.Monad
import System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified GHC.IO.Handle.Types as H

--col1 = "#fcfcfc"
--col2 = "#252525"
--col3 = "#C0392B"
--col4 = "#27ae60"
--col5 = "#f39c12"
--col6 = "#2980b9"
--col7 = "#8e44ad"
--col8 = "#16a085"

myblack0  = "#050505"
myblack1  = "#151515"
myblack2  = "#212121"
myblack3  = "#505050"
myred     = "#ac2122"
mygreen   = "#7e8e50"
myyellow  = "#e5b567"
myblue1   = "#4c799b"
myblue2   = "#6c99bb"
mymagenta = "#9f4e85"
mycyan    = "#7dd6cf"
mywhite1  = "#d0d0d0"
mywhite2  = "#f5f5f5"

myCurrentWS = mymagenta
myOtherScreenWS = myblack3
myActiveWS = myblack2



bar1 = "dzen2 -dock -p -ta l -e 'button3=' -fn 'Deja Vu Mono 2-9' -fg '" ++ mywhite1 ++ "' -bg '" ++ myblack2 ++ "' -h 20 -w 750"
bar2 = "sh /home/augusto/.xmonad/scripts/dzen_info_1.sh"
cal_ic = "^ca(1,xdotool key alt+space)^i(/home/augusto/.xmonad/icons/"
vtitle = "^bg(" ++ myblack3 ++ ")    "
vtitle_end = "    ^bg()"

startup = do
    spawnOnce "sh /home/augusto/.xmonad/scripts/autostart.sh"

myLogHook h = do
    dynamicLogWithPP $ tryPP h

tryPP :: Handle -> PP
tryPP h = def
    { ppOutput    = hPutStrLn h

    , ppCurrent   = dzenColor mywhite1 myCurrentWS . pad
    , ppVisible   = dzenColor mywhite1 myOtherScreenWS . pad
    , ppHidden    = dzenColor mywhite1 myActiveWS . pad
    --, ppHiddenNoWindows = dzenColor mywhite1 myblack0 . pad
    , ppWsSep   = " "
    , ppSep     = ""

    , ppTitle   = wrap vtitle vtitle_end . shorten 60
    , ppLayout    = dzenColor mywhite1 myblack1 . pad .
      (\t -> case t of
              "Spacing Grid"        -> "  " ++ cal_ic ++ "grid.xbm)  ^ca()"
              "Spacing Spiral"      -> "  " ++ cal_ic ++ "spiral.xbm)  ^ca()"
              --"Spacing Circle"      -> "  " ++ cal_ic ++ "circle.xbm)  ^ca()"
              "Spacing Tall"        -> "  " ++ cal_ic ++ "sptall.xbm)  ^ca()"
              "Mirror Spacing Tall" -> "  " ++ cal_ic ++ "mptall.xbm)  ^ca()"
              "Full"                -> "  " ++ cal_ic ++ "full.xbm)  ^ca()"
              _                     -> " ? "
      )
    , ppOrder   = \(ws:l:t:_) -> [l,"^ca(5,xdotool key alt+Page_Down)^ca(4,xdotool key alt+Page_Up)" ++ ws ++ "^ca()^ca()"," | "++t]
    }

myExtraWS = [("0", xK_0),("-", xK_minus),("=", xK_equal)]

clickableWorkSpace ws key = "^ca(1,xdotool key alt+" ++ key ++ ")" ++ ws ++ "^ca()"

myWorkspaces :: [String]
myWorkspaces = zipWith clickableWorkSpace
    ([ "1:TERM" , "2:WEB"] ++ map show [3..7] ++ ["8:IM" , "9:ENT"] ++ map fst myExtraWS)
    (map show [1..9] ++ map fst myExtraWS)

myKeys = [((mod1Mask, xK_p), spawn "dmenu_run -i") -- case insensitive
         , ((mod1Mask, xK_f), withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1))
         , ((mod1Mask, xK_q), spawn "killall dzen2; killall stalonetray; xmonad --recompile; xmonad --restart")
         , ((0, xK_Print), spawn "scrot -s -e 'mv $f ~/Pictures/scrots/'")


         , ((mod1Mask, xK_Page_Up), moveTo Prev NonEmptyWS)
         , ((mod1Mask, xK_Page_Down), moveTo Next NonEmptyWS)
         , ((mod1Mask .|. shiftMask, xK_Page_Up), prevWS)
         , ((mod1Mask .|. shiftMask, xK_Page_Down), nextWS)

         --, ((mod1Mask .|. shiftMask, xK_space), sendMessage PrevLayout) TODO Is this impossible?

         , ((0, xF86XK_AudioLowerVolume ), spawn "amixer set Master 2%-")
         , ((0, xF86XK_AudioRaiseVolume ), spawn "amixer set Master 2%+")
         , ((0, xF86XK_AudioMute ), spawn "amixer set Master toggle")
         ]
         ++
         [ ((mod1Mask, key), windows $ W.greedyView (clickableWorkSpace ws ws)) | (ws, key) <- myExtraWS ]
         ++
         [ ((mod1Mask .|. shiftMask, key), windows $ W.shift (clickableWorkSpace ws ws)) | (ws, key) <- myExtraWS ]

myLayout = avoidStruts $ smartBorders ( sTall ||| sGrid ||| Mirror sTall ||| Full )
    where
      sTall = spacingRaw True myBorder True myBorder True $ Tall 1 (5/100) (1/2)
      sGrid = spacingRaw True myBorder True myBorder True Grid
      --sCircle = spacing 5 $ Circle
      --sSpiral = spacingRaw True myBorder True myBorder True $ spiral (toRational (2/(1 + sqrt 5::Double)))
      myBorder = Border 5 5 5 5

myApps = composeAll
    [ className =? "mpv" --> doFloat
    , className =? "TelegramDesktop"  <&&> title =? "Media viewer" --> doFloat
    ]

main = do
    leftBar <- spawnPipe bar1
    rightBar <- spawnPipe bar2
    stalone <- spawn "sleep 0.5; stalonetray"

    xmonad $ def
      { manageHook = myApps <+> manageDocks <+> manageHook def
      , layoutHook = myLayout
      , handleEventHook = docksEventHook <+> handleEventHook def
      , modMask = mod1Mask
      , workspaces = myWorkspaces
      , terminal  = "urxvt -e tmux"
      , focusedBorderColor = myblue2
      , normalBorderColor = "" ++ myblack2 ++ ""
      , borderWidth = 2
      , startupHook = startup <+> setWMName "LG3D" <+> docksStartupHook
      , logHook = fadeInactiveLogHook 0.75 <+> myLogHook leftBar
      } `additionalKeys` myKeys
