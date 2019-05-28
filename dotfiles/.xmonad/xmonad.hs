import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.Magnifier

import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO
import qualified XMonad.StackSet as W
import Data.Ratio ((%))
import Data.Monoid((<>))

myblack1  = "#151515"
myblack2  = "#212121"
myblack3  = "#505050"
myblue2   = "#6c99bb"
mymagenta = "#9f4e85"
mywhite0  = "#a0a0a0"
mywhite1  = "#d0d0d0"

myCurrentWS = mymagenta
myOtherScreenWS = myblack3
myActiveWS = myblack2

-- Note the simple quotes!
bar1 = "dzen2 -dock -p -ta l -e 'button3=' -fn 'InputMono-10' -fg '"
  ++ mywhite1 ++ "' -bg '" ++ myblack2 ++ "' -h 25 -w 900"

conkyBar = "conky -c ~/.xmonad/scripts/dzenconky_1 | "
  ++ "dzen2 -dock -p -ta r -e 'button3=' -fn 'InputMono-10' -fg '"
  ++ mywhite1 ++ "' -bg '" ++ myblack2 ++ "' -h 25 -w 500 -x 866 -y 0"

myLogHook h =
    dynamicLogWithPP $ tryPP h

-- TODO How to set color based on a given screen (for having multiple docks)
-- We can set the color of ppCurrent and ppVisible with the same function
-- that sets the color based on whether the WS is the one in the screen or not.
tryPP :: Handle -> PP
tryPP h = def
    { ppOutput    = hPutStrLn h
    , ppCurrent   = dzenColor mywhite1 myCurrentWS . clickableWS . pad
    , ppVisible   = dzenColor mywhite1 myOtherScreenWS . clickableWS . pad
    , ppHidden    = dzenColor mywhite1 myActiveWS . clickableWS . pad
    --, ppHiddenNoWindows = dzenColor mywhite1 myblack0 . pad
    , ppWsSep   = ""
    , ppSep     = ""

    , ppTitle   = wrap vtitle vtitleEnd . shorten 60
    , ppLayout    = dzenColor mywhite1 myblack1 . pad .
      (\t -> calIc ++ (case cleanLayout t of
              "Spacing Tall"  -> "sptall.xbm"
              "Full"          -> "full.xbm"
              "Spacing Grid"  -> "grid.xbm"
              _               -> "grid.xbm" -- Need this because of WS 0
        ) ++  ") ^ca()" )
    , ppOrder = \(ws:l:t:_) ->
        [ l
        , clickable "5" "alt+Page_Down" $ clickable "4" "alt+Page_Up" ws
        , dzenColor mywhite0 mywhite0 " "
        , t]
    }
  where
  cleanLayout = drop (length "ReflectX IM ReflextX ")
  calIc = "^ca(1,xdotool key alt+space)^i(/home/augusto/.xmonad/icons/"
  vtitle = "^bg(" ++ myblack3 ++ ")  "
  vtitleEnd = "  ^bg()"
  clickable mouseKey action string =
    "^ca("++mouseKey++",xdotool key "++action++")" ++ string ++ "^ca()"
  -- TODO this wont work with - or =, need to check if xdotool understand the
  -- first character (for - is xK_minus and so on)
  clickableWS ws = "^ca(1,xdotool key alt+" ++ [ws!!1] ++ ")" ++ ws ++ "^ca()" -- 2nd because of padding space


myExtraWS = [("0", xK_0),("-", xK_minus),("=", xK_equal)]

myWorkspaces :: [String]
myWorkspaces =
    [ "1:TERM" , "2:WEB"] ++ map show [3..7] ++ ["8:IM" , "9:ENT"] ++ map fst myExtraWS

myKeys = [ ((mod1Mask, xK_p), spawn "dmenu_run -i -m 0 -fn 'InputMono-10'") -- case insensitive
         , ( (mod1Mask, xK_f)
           , withFocused $ windows . flip W.float (W.RationalRect 0 0 1 1))
         , ( (mod1Mask, xK_q)
           , spawn $ "killall dzen2; killall stalonetray;"
             <> "xmonad --recompile; xmonad --restart")

         , ((0, xK_Print), spawn "scrot")
         --, ((controlMask, xK_Print), spawn "gnome-screenshot --clipboard")
         , ((shiftMask, xK_Print), spawn "sleep 0.3; scrot -s")
         --, ((controlMask, xK_Print), spawn "gnome-screenshot --area")


         , ((mod1Mask, xK_Page_Up), moveTo Prev NonEmptyWS)
         , ((mod1Mask, xK_Page_Down), moveTo Next NonEmptyWS)
         , ((mod1Mask .|. shiftMask, xK_Page_Up), prevWS)
         , ((mod1Mask .|. shiftMask, xK_Page_Down), nextWS)

         --, ((mod1Mask .|. shiftMask, xK_space), sendMessage PrevLayout) TODO Is this impossible?
         --
         , ((controlMask .|. shiftMask, xK_space), spawn "~/.au.conf/scripts/toogleKeyboardLayout")

         , ((0, xF86XK_AudioLowerVolume ), spawn "amixer set Master 2%-")
         , ((0, xF86XK_AudioRaiseVolume ), spawn "amixer set Master 2%+")
         , ((0, xF86XK_AudioMute ), spawn "amixer set Master toggle")
         ]
         ++
         [ ((mod1Mask, key), windows $ W.greedyView ws) | (ws, key) <- myExtraWS ]
         ++
         [ ((mod1Mask .|. shiftMask, key), windows $ W.shift ws) | (ws, key) <- myExtraWS ]

myLayout = avoidStruts $ smartBorders $
    onWorkspace "0" sGrid $
    reflectHoriz $ withIM (67%256) (ClassName "TelegramDesktop") $ reflectHoriz
    ( magnifiercz 1.03 sTall ||| Full)
    where
      sTall = spacingRaw True myBorder True myBorder True $ Tall 1 (2/100) (1/2)
      sGrid = spacingRaw True myBorder True myBorder True Grid
      myBorder = Border 5 5 5 5

-- use xprop to get these properties
myApps = composeAll
    [ isFullscreen                                                 --> doFullFloat
    , className =? "mpv"                                           --> doFloat
    , className =? "TelegramDesktop"  <&&> title =? "Media viewer" --> doFloat
    , title =? "Open File"                                         --> doCenterFloat
    , title =? "File Operation Progress"                           --> doCenterFloat
    --, resource =? "Dialog"                                         --> doFloat
    , title =? "CSSE101Queue"                                  --> doFloat
    , title =? "tk"                                            --> doFloat
    ]

main = do
    leftBar <- spawnPipe bar1
    _ <- spawn conkyBar
    _ <- spawn "sleep 0.5; stalonetray"

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
      , startupHook = setWMName "LG3D" <+> docksStartupHook
      , logHook = fadeInactiveLogHook 0.9 <+> myLogHook leftBar
      } `additionalKeys` myKeys
