import XMonad
--import XMonad.Util.ExtensibleState as XS
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.Magnifier
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt

import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import Graphics.X11.ExtraTypes.XF86
import System.IO
import System.Exit

import qualified XMonad.StackSet as W

import Control.Monad
import Data.Ratio ((%))
import Data.Maybe
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
-- bar1 = "dzen2 -dock -p -ta l -e 'button3=' -fn 'InputMono-10' -fg '"
--   ++ mywhite1 ++ "' -bg '" ++ myblack2 ++ "' -h 25 -w 800"
--
-- conkyBar = "conky -c ~/.xmonad/scripts/dzenconky_1 | "
--   ++ "dzen2 -dock -p -ta r -e 'button3=' -fn 'InputMono-10' -fg '"
--   ++ mywhite1 ++ "' -bg '" ++ myblack2 ++ "' -h 25 -w 566 -x 800 -y 0"
--
-- bar2 = "dzen2 -dock -p -ta l -e 'button3=' -fn 'InputMono-10' -fg '"
--   ++ mywhite1 ++ "' -bg '" ++ myblack2 ++ "' -h 25 -w 900 -x 1366"
--
-- conkyBar2 = "conky -c ~/.xmonad/scripts/dzenconky_1 | "
--   ++ "dzen2 -dock -p -ta r -e 'button3=' -fn 'InputMono-10' -fg '"
--   ++ mywhite1 ++ "' -bg '" ++ myblack2 ++ "' -h 25 -w 500 -x 2232 -y 0"

myLogHook :: [(Handle, ScreenId)] -> X ()
myLogHook = mapM_ (dynamicLogWithPP <=< tryPP)


tryPP :: (Handle, ScreenId) -> X PP
tryPP (h, sid) = do
  Just wid <- screenWorkspace sid
  return $ def
    { ppOutput    = hPutStrLn h
    , ppCurrent = visibleWS wid
    , ppVisible = visibleWS wid
    , ppHidden    = dzenColor mywhite1 myActiveWS . clickableWS . pad
    , ppWsSep   = ""
    , ppSep     = ""

    , ppTitle   = wrap vtitle vtitleEnd . shorten 60
    , ppLayout    = dzenColor mywhite1 myblack1 . pad .
      (\t -> calIc ++ (case last . words $ t of
              "Tall"  -> "sptall.xbm"
              "Full"  -> "full.xbm"
              "Grid" -> "grid.xbm"
              _      -> "grid.xbm" -- Need this because of WS 0
        ) ++  ")^ca()")
    , ppOrder = \(ws:l:t:_) ->
        [ l
        , clickable "5" "alt+Page_Down" $ clickable "4" "alt+Page_Up" ws
        , dzenColor mywhite0 mywhite0 " "
        , t]
    }
  where
  visibleWS wid ws = if wid == ws
    then dzenColor mywhite1 myCurrentWS . clickableWS . pad $ ws
    else dzenColor mywhite1 myOtherScreenWS . clickableWS . pad $ ws
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

myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:fira code:size=12"
  }

myKeys = [ ((mod1Mask .|. shiftMask, xK_q), confirmPrompt myXPConfig "exit" $ io exitSuccess)
         , ((mod1Mask, xK_p), spawn "rofi -combi-modi window,drun,run -show combi -modi combi,drun -font 'InputMono 10' -show-icons -theme solarized") -- case insensitive
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
    ( magnifiercz 1.02 sTall ||| Full)
    where
      sTall = spacingRaw True myBorder True myBorder True $ Tall 1 (2/100) (1/2)
      sGrid = spacingRaw True myBorder True myBorder True Grid
      myBorder = Border 5 5 5 5

-- use xprop to get these properties
myApps = composeAll $
    [ isFullscreen                                             --> doFullFloat
    , className =? "mpv"                                           --> doFloat
    , className =? "TelegramDesktop"  <&&> title =? "Media viewer" --> doFloat
    --, resource =? "Dialog" --> doFloat
    ]
    ++ [ title =? t --> doCenterFloat | t <- centerFloatTitles ]
    ++ [ className =? n -->
      placeHook (withGaps (25,0,0,0) $ underMouse (0.2, 0)) <+> doFloat
          | n <- mouseFloatClassNames ]
  where
    centerFloatTitles =
      [ "Open File"
      , "File Operation Progress"
      , "Volume Control"
      ]
    mouseFloatClassNames =
      [ "MEGAsync"
      , "Toggl Desktop"
      ]

main = do
    --leftBar <- spawnPipe bar1
    --rightBar <- spawnPipe bar2
    --spawn conkyBar
    --spawn conkyBar2
    --spawn "sleep 1.5; stalonetray"

    xmonad $ ewmh def
      { manageHook = myApps <+> manageDocks <+> manageHook def
      , layoutHook = myLayout
      , handleEventHook = docksEventHook <+> handleEventHook def
      , modMask = mod1Mask
      , workspaces = myWorkspaces
      , terminal  = "urxvt -e tmux"
      , focusedBorderColor = myblue2
      , normalBorderColor = "" ++ myblack2 ++ ""
      , borderWidth = 2
      , startupHook = setWMName "LG3D"
                    <+> docksStartupHook
      , logHook = fadeInactiveLogHook 0.9 -- <+> myLogHook [ (leftBar, S 0)
                                            --            , (rightBar, S 1)]
      } `additionalKeys` myKeys
