import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS ( WSType(NonEmptyWS), moveTo, prevWS, nextWS
                              , Direction1D(Next, Prev) )
import XMonad.Hooks.DynamicBars ( dynStatusBarStartup, dynStatusBarEventHook
                                , multiPP )
import XMonad.Hooks.DynamicLog ( PP(..), shorten, pad )
import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.FadeInactive ( fadeInactiveLogHook )
import XMonad.Hooks.ManageDocks ( avoidStruts, docksEventHook, docksStartupHook
                                , manageDocks )
import XMonad.Hooks.ManageHelpers ( doCenterFloat, doFullFloat, isFullscreen )
import XMonad.Hooks.Place ( placeHook, withGaps, underMouse )
import XMonad.Hooks.SetWMName ( setWMName )
import XMonad.Layout.Grid ( Grid(Grid) )
import XMonad.Layout.IM ( withIM, Property(ClassName) )
import XMonad.Layout.NoBorders ( lessBorders, Ambiguity(OnlyScreenFloat) )
import XMonad.Layout.PerWorkspace ( onWorkspace )
import XMonad.Layout.Reflect ( reflectHoriz )
import XMonad.Layout.Spacing ( toggleScreenSpacingEnabled
                             , toggleWindowSpacingEnabled, Border(Border)
                             , spacingRaw )
import XMonad.Prompt ( XPConfig(..), XPPosition(Top) )
import XMonad.Prompt.ConfirmPrompt ( confirmPrompt )
import XMonad.Util.EZConfig ( additionalKeys )
import XMonad.Util.NamedWindows ( getName )
import XMonad.Util.Run ( spawnPipe, safeSpawn )

import Au.Workspaces ( myWorkspaces, myExtraWS )
import Au.Colors ( color, withAlpha )
import Au.Util.Screenshot (screenshot)
import Au.Util.Polybar

import Graphics.X11.ExtraTypes.XF86 ( xF86XK_AudioLowerVolume
                                    , xF86XK_AudioRaiseVolume
                                    , xF86XK_AudioMute )
import System.Exit (exitSuccess)
import System.IO
import System.Posix.Files

import Data.Ratio ((%))
import Data.Monoid((<>))
import Text.Printf(printf)

myLogHook :: X ()
myLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  multiPP pP' pP'
  let title' = clickable ScrollUp "xdotool key alt+k" $
               clickable ScrollDown "xdotool key alt+j" $
               background (withAlpha "aa" $ color 0) $
               padUntil 52 $
               take 50 title
  titleLogHook (S 0) title'
  titleLogHook (S 1) title'
  where
    padUntil :: Int -> String -> String
    padUntil l s
      | length s >= l = s
      | otherwise =
          let dif = l - length s
           in (replicate (dif `div` 2) ' ') <> s <>
              (replicate ((dif+1) `div` 2) ' ')

pP' :: PP
pP' = tryPP undefined

tryPP :: Handle -> PP
tryPP h = def
    { ppOutput  = hPutStrLn h
    , ppCurrent = foreground (color 15) . underline (color 12) . clickableWS
    , ppVisible = underline (withAlpha "70" $ color 4) . clickableWS
    , ppHidden  = clickableWS
    , ppHiddenNoWindows = foreground (withAlpha "35" (color 15)) . clickableWS
    , ppWsSep   = " "
    , ppSep     = ""
    , ppTitle   = shorten 60
    , ppLayout  = const " "
    , ppOrder = \(ws:l:t:_) ->
        [ background (withAlpha "aa" $ color 0) $ pad $ scrollableWS ws ]
    }
  where
  clickableWS ws = clickable LeftClick ("xdotool key alt+" ++ [head ws]) ws
  scrollableWS wss = clickable ScrollUp "xdotool key super+k" $
                     clickable ScrollDown "xdotool key super+j" $
                     wss


myXPConfig :: XPConfig
myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:fira code:size=12"
  }

superMask = mod4Mask
myKeys = [ ((mod1Mask .|. shiftMask, xK_q), confirmPrompt myXPConfig "exit" $ io exitSuccess)
         , ((mod1Mask, xK_p), spawn "rofi -m -4 -combi-modi drun,run -show combi -modi combi,window -font 'InputMono 10' -show-icons")
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
         , ((mod1Mask, xK_c), spawn "~/.au.conf/scripts/toggleCompton")

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
    [ isFullscreen                                            --> doFullFloat
    , className =? "TelegramDesktop" <&&> title =? "Media viewer" --> doFloat
    ,  windowRole =? "pop-up"                                 --> doCenterFloat
    ]
    ++ [ title =? t     --> doCenterFloat   | t <- centerFloatTitles ]
    ++ [ className =? n --> doCenterFloat   | n <- centerFloatClassNames ]
    ++ [ className =? n --> floatUnderMouse | n <- mouseFloatClassNames ]
  where
    windowRole = stringProperty "WM_WINDOW_ROLE"
    floatUnderMouse =
      placeHook (withGaps (25,0,0,0) $ underMouse (0.2, 0)) <+> doFloat
    centerFloatTitles =
      [ "Open File"
      , "File Operation Progress"
      , "Volume Control"
      ]
    centerFloatClassNames =
      [ "Wpg"
      ]
    mouseFloatClassNames =
      [ "MEGAsync"
--      , "Toggl Desktop"
      , "mpv"
      ]

-- dynStatusBarEventHook expects a `ScreenId -> IO Handle`
barInScreen :: ScreenId -> IO Handle
barInScreen (S 0) = do
    spawn "sleep 1; polybar bar1"
    spawnPipe "cat >> /tmp/.xmonad-workspace-log"
barInScreen (S 1) = do
    spawn "sleep 1; polybar barhdmi"
    spawnPipe "cat >> /tmp/.xmonad-workspace-hdmi-log"

titleLogHook :: ScreenId -> String -> X ()
titleLogHook screen s =
  spawn $ printf "echo \"%s\n\" >> %s" s (screenPipe screen)
  where
    screenPipe (S 0) = "/tmp/.xmonad-title-log"
    screenPipe (S 1) = "/tmp/.xmonad-title-hdmi-log"

startUpBeep :: IO ()
startUpBeep =
    return ()
    --spawn "beep -f 165.4064 -l 100 -n -f 130.813 -l 100 -n -f 261.626 -l 100 -n -f 523.251 -l 100 -n -f1046.50 -l 100 -n -f 2093.00 -l 100 -n -f 4186.01 -l 200"

main = do
    startUpBeep
    xmonad $ ewmh def
      { manageHook = myApps <+> manageDocks <+> manageHook def
      , layoutHook = myLayout
      , handleEventHook = docksEventHook
                        <+> dynStatusBarEventHook barInScreen (spawn "killall polybar")
                        <+> handleEventHook def
      , modMask = mod1Mask
      , workspaces = myWorkspaces
      , terminal  = "urxvt -e tmux"
      , focusedBorderColor = color 4
      , normalBorderColor = color 0
      , borderWidth = 2
      , startupHook = setWMName "LG3D" -- TODO what is this?
                    <+> docksStartupHook
                    <+> dynStatusBarStartup barInScreen (return ())
      , logHook = myLogHook
      } `additionalKeys` myKeys
