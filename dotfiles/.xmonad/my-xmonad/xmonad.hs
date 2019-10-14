import XMonad
--import XMonad.Util.ExtensibleState as XS
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicBars
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
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (spawnPipe, safeSpawn)
import Graphics.X11.ExtraTypes.XF86
import System.IO
import System.Posix.Files
import System.Exit
import           System.Process                 ( callCommand )

import qualified XMonad.StackSet as W

import Control.Monad
import Data.Function (on)
import Data.List (sortBy)
import Data.Ratio ((%))
import Data.Maybe
import Data.Monoid((<>))

myblack1  = "#151515"
myblack2  = "#212121"
myblack3  = "#505050"
myblue2   = "#7ca9cb"
myblue3   = "#3c698b"
mymagenta = "#9f4e85"
mywhite0  = "#a0a0a0"
mywhite1  = "#d0d0d0"

myCurrentWS = myblue2
myOtherScreenWS = myblue3
hiddenWS = myblack3
-- myActiveWS = myblack2

myLogHook :: X ()
myLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  multiPP pP' pP'
  spawn $ "echo \"" ++ (take 50 title ++ "\n") ++ "\" >> /tmp/.xmonad-title-log"
  spawn $ "echo \"" ++ (take 50 title ++ "\n") ++ "\" >> /tmp/.xmonad-title-hdmi-log"

underline color s = "%{u" ++ color ++ "}%{+u}" ++ s ++ "%{-u}"
overline color s = "%{o" ++ color ++ "}%{+o}" ++ s ++ "%{-o}"

pP' :: PP
pP' = tryPP undefined

tryPP :: Handle -> PP
tryPP h = def
    { ppOutput  = hPutStrLn h
    , ppCurrent = pad . underline myCurrentWS . clickableWS
    , ppVisible = pad . underline myOtherScreenWS . clickableWS
    , ppHidden  = pad . clickableWS
    , ppHiddenNoWindows = pad . foreGround hiddenWS . clickableWS
    , ppWsSep   = ""
    , ppSep     = ""
    , ppTitle   = shorten 60
    , ppLayout  = const " "
    , ppOrder = \(ws:l:t:_) ->
        [ scrollableWS ws ]
    }
  where
  foreGround color s = "%{F" ++ color ++ "}" ++ s ++ "%{F-}"
  clickableWS ws = "%{A1:xdotool key alt+" ++ [head ws] ++ ":}" ++ ws ++ "%{A}"
  scrollableWS wss = "%{A4:xdotool key super+k:}" ++
                     "%{A5:xdotool key super+j:}" ++
                     wss ++ "%{A}%{A}"

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

screenshot :: Bool -> Bool -> X ()
screenshot sel cop = spawn $ "sleep 0.3; maim " ++ selec ++ copy ++
                      "~/Pictures/Screenshots/$(date +%s).png"
  where
    selec = if sel then "-s " else ""
    copy = if cop
              then "| xclip -selection clipboard -t image/png; " ++
                   "xclip -selection clipboard -o > "
              else ""


superMask = mod4Mask
myKeys = [ ((mod1Mask .|. shiftMask, xK_q), confirmPrompt myXPConfig "exit" $ io exitSuccess)
         , ((mod1Mask, xK_p), spawn "rofi -m -4 -combi-modi drun,run -show combi -modi combi,window -font 'InputMono 10' -show-icons -theme solarized")
         , ( (mod1Mask, xK_f)
           , withFocused $ windows . flip W.float (W.RationalRect 0 0 1 1))
         , ( (mod1Mask, xK_q)
           , spawn $ "killall polybar; sleep 1"
             <> "xmonad --recompile; xmonad --restart")

         , ((0, xK_Print), screenshot False False)
         , ((controlMask, xK_Print), screenshot False True)
         , ((shiftMask, xK_Print),  screenshot True False)
         , ((controlMask .|. shiftMask, xK_Print), screenshot True True)

         , ((superMask, xK_k), moveTo Prev NonEmptyWS)
         , ((superMask, xK_j), moveTo Next NonEmptyWS)
         , ((superMask .|. shiftMask, xK_k), prevWS)
         , ((superMask .|. shiftMask, xK_j), nextWS)

         , ((mod1Mask .|. shiftMask, xK_b), toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled)

         --, ((mod1Mask .|. shiftMask, xK_space), sendMessage PrevLayout) TODO Is this impossible?
         --
         , ((controlMask .|. shiftMask, xK_space), spawn "~/.au.conf/scripts/toggleKeyboardLayout")

         , ((0, xF86XK_AudioLowerVolume ), spawn "amixer set Master 2%-")
         , ((0, xF86XK_AudioRaiseVolume ), spawn "amixer set Master 2%+")
         , ((0, xF86XK_AudioMute ), spawn "amixer set Master toggle")
         ]
         ++
         [ ((mod1Mask, key), windows $ W.greedyView ws) | (ws, key) <- myExtraWS ]
         ++
         [ ((mod1Mask .|. shiftMask, key), windows $ W.shift ws) | (ws, key) <- myExtraWS ]

myLayout = lessBorders OnlyScreenFloat $ avoidStruts $
    spacingRaw False myBorder True myBorderIn True $
    onWorkspace "0" Grid $
    reflectHoriz $ withIM (71%256) (ClassName "TelegramDesktop") $ reflectHoriz
    (sTall ||| Full)
    where
      sTall = Tall 1 (2/100) (1/2)
      myBorder = Border 1 1 1 1
      myBorderIn = Border 8 8 8 8

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

barInScreen :: ScreenId -> IO Handle
barInScreen (S 0) = do
    spawn "sleep 1; polybar bar1"
    spawnPipe "cat >> /tmp/.xmonad-workspace-log"
barInScreen (S 1) = do
    spawn "sleep 1; polybar barhdmi"
    spawnPipe "cat >> /tmp/.xmonad-workspace-hdmi-log"

main = do
    spawn "beep -f 165.4064 -l 100 -n -f 130.813 -l 100 -n -f 261.626 -l 100 -n -f 523.251 -l 100 -n -f1046.50 -l 100 -n -f 2093.00 -l 100 -n -f 4186.01 -l 200"
    xmonad $ ewmh def
      { manageHook = myApps <+> manageDocks <+> manageHook def
      , layoutHook = myLayout
      , handleEventHook = docksEventHook
                        <+> dynStatusBarEventHook barInScreen (spawn "killall polybar")
                        <+> handleEventHook def
      , modMask = mod1Mask
      , workspaces = myWorkspaces
      , terminal  = "urxvt -e tmux"
      , focusedBorderColor = myblue2
      , normalBorderColor = "" ++ myblack2 ++ ""
      , borderWidth = 2
      , startupHook = setWMName "LG3D"
                    <+> docksStartupHook
                    <+> dynStatusBarStartup barInScreen (return ())
      , logHook = fadeInactiveLogHook 0.9
                <+> myLogHook
      } `additionalKeys` myKeys
