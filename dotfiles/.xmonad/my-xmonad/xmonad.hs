{-# LANGUAGE RecordWildCards #-}
import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS ( WSType(WSIs), moveTo, prevWS, nextWS
                              , Direction1D(Next, Prev) )
--import XMonad.Actions.UpdateFocus ( focusOnMouseMove, adjustEventInput )
import XMonad.Actions.FloatKeys
import XMonad.Actions.Navigation2D
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
import XMonad.Layout.BoringWindows
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.NoBorders ( lessBorders, Ambiguity(OnlyScreenFloat) )
import XMonad.Layout.PerWorkspace ( onWorkspace )
import XMonad.Layout.Reflect ( reflectHoriz )
import XMonad.Layout.Spacing ( toggleScreenSpacingEnabled
                             , toggleWindowSpacingEnabled, Border(Border)
                             , spacingRaw )
import XMonad.Layout.TrackFloating ( trackFloating, useTransientFor )
import XMonad.Prompt ( XPConfig(..), XPPosition(Top) )
import XMonad.Prompt.ConfirmPrompt ( confirmPrompt )
import XMonad.Util.EZConfig ( additionalKeys )
import XMonad.Util.NamedWindows ( getName )
import XMonad.Util.NamedScratchpad ( NamedScratchpad(..), namedScratchpadAction
                                   , namedScratchpadManageHook, defaultFloating )
import XMonad.Util.Run ( spawnPipe, safeSpawn )

import Au.Colors ( withAlpha, polybarBGAlpha )
import Au.Util.Screenshot (screenshot)
import Au.Util.Polybar

import Graphics.X11.ExtraTypes.XF86 ( xF86XK_AudioLowerVolume
                                    , xF86XK_AudioRaiseVolume
                                    , xF86XK_AudioMute, xF86XK_AudioPlay
                                    , xF86XK_AudioNext, xF86XK_AudioPrev )
import System.Exit (exitSuccess)
import System.IO
import System.Posix.Files

import Data.Ratio ((%))
import Data.Monoid ((<>))
import Data.List ( isSuffixOf )
import Data.Maybe ( isJust )
import Text.Printf (printf)


import XMonad.Layout.Tabbed
import XMonad.Layout.Named
import XMonad.Actions.ConditionalKeys
import XMonad.Layout.ShowWName
import XMonad.Layout.MouseResizableTile
import XMonad.Hooks.InsertPosition

color8 = "#4C566A"
color12 = "#81A1C1"
color6 = "#88C0D0"
color15 = "#ECEFF4"
color0 = "#3B4252"
colorbackground = "#2E3440"

myLogHook :: X ()
myLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  multiPP pP' pP'
  let title' = clickable ScrollUp "xdotool key alt+k" $
               clickable ScrollDown "xdotool key alt+j" $
               background (withAlpha polybarBGAlpha $ colorbackground) $
               padUntil 48 $
               take 46 title
  titleLogHook (S 0) title'
  titleLogHook (S 1) title'
  where
    padUntil :: Int -> String -> String
    padUntil l s
      | length s >= l = s
      | otherwise =
          let dif = l - length s
           in replicate (dif `div` 2) ' ' <> s <>
              replicate ((dif+1) `div` 2) ' '

pP' :: PP
pP' = tryPP undefined

tryPP :: Handle -> PP
tryPP h = def
    { ppOutput  = hPutStrLn h
    , ppCurrent = underline (color12) . clickableWS . noNSP
    , ppVisible = underline (withAlpha "70" $ color6) . clickableWS . noNSP
    , ppHidden  = clickableWS . noNSP
    , ppHiddenNoWindows = foreground (withAlpha "50" (color15)) . clickableWS . noNSP
    , ppWsSep   = " "
    , ppSep     = ""
    , ppTitle   = shorten 60
    , ppLayout  = id
    , ppOrder = \(ws:l:t:_) ->
        [pad $ scrollableWS ws ]
    }
  where
  noNSP "NSP" = ""
  noNSP s = s
  clickableWS ws = clickable LeftClick ("xdotool key alt+" ++ [head ws]) ws
  scrollableWS wss = clickable ScrollUp "xdotool key super+k" $
                     clickable ScrollDown "xdotool key super+j"
                     wss


myXPConfig :: XPConfig
myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "xft:fira code:size=12:style=Bold"
  , height            = 24
  }

superMask = mod4Mask
myKeys = [ ((mod1Mask .|. shiftMask, xK_q), confirmPrompt myXPConfig "exit" $ io exitSuccess)
         , ((mod1Mask, xK_p), spawn "rofi -m -4 -combi-modi drun,run -show combi -modi combi,window -show-icons")
         , ( (mod1Mask, xK_f)
           , withFocused $ windows . flip W.float (W.RationalRect 0 0 1 1))
         , ( (mod1Mask, xK_q)
           , spawn $ "killall polybar; killall tint2; sleep 1"
             <> "xmonad --recompile; xmonad --restart")

         , ((0, xK_Print), screenshot False False)
         , ((controlMask, xK_Print), screenshot False True)
         , ((shiftMask, xK_Print),  screenshot True False)
         , ((controlMask .|. shiftMask, xK_Print), screenshot True True)

         --, ((superMask, xK_Tab), moveTo Next notNSPnonEmpty)
         --, ((superMask .|. shiftMask, xK_Tab), moveTo Prev notNSPnonEmpty)
         , ((superMask, xK_j), moveTo Next notNSPnonEmpty)
         , ((superMask, xK_k), moveTo Prev notNSPnonEmpty)
         , ((superMask, xK_j), moveTo Next notNSPnonEmpty)
         , ((superMask .|. shiftMask, xK_k), prevWS)
         , ((superMask .|. shiftMask, xK_j), nextWS)

         --, ((mod1Mask .|. shiftMask, xK_b), toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled)


         --, ((mod1Mask .|. shiftMask, xK_space), sendMessage PrevLayout) TODO Is this impossible?
         --
         , ((controlMask .|. shiftMask, xK_space), spawn "~/.au.conf/scripts/toggleKeyboardLayout")
         , ((mod1Mask, xK_c), spawn "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'")
         , ((superMask, xK_c), spawn "~/.au.conf/scripts/toggleCompton")

         , ((0, xF86XK_AudioLowerVolume ), spawn "amixerdunst 2%-")
         , ((0, xF86XK_AudioRaiseVolume ), spawn "amixerdunst 2%+")
         , ((0, xF86XK_AudioMute ), spawn "amixerdunst toggle")

         , ((0, xF86XK_AudioPlay ), spawn "playerctl -p spotify play-pause")
         , ((0, xF86XK_AudioNext ), spawn "playerctl -p spotify next")
         , ((0, xF86XK_AudioPrev ), spawn "playerctl -p spotify previous")
         , ((mod1Mask .|. shiftMask, xK_p), spawn "playerctl -p spotify play-pause") -- TODO temporal
         , ((mod1Mask .|. shiftMask, xK_Right), spawn "playerctl -p spotify next") -- TODO temporal
         , ((mod1Mask .|. shiftMask, xK_Left), spawn "playerctl -p spotify previous") -- TODO temporal

         -- scratchpads and floating windows
         --, ((mod1Mask .|. controlMask, xK_t), namedScratchpadAction myScratchpads "Habitica")
         --, ((mod1Mask .|. controlMask, xK_m), namedScratchpadAction myScratchpads "RememberTheMilk")
         , ((mod1Mask .|. controlMask, xK_s), namedScratchpadAction myScratchpads "Conky")

         , ((mod1Mask .|. controlMask, xK_k), withFocused (keysMoveWindow (0,-10)))
         , ((mod1Mask .|. controlMask, xK_j), withFocused (keysMoveWindow (0,10)))
         , ((mod1Mask .|. controlMask, xK_l), withFocused (keysMoveWindow (10,0)))
         , ((mod1Mask .|. controlMask, xK_h), withFocused (keysMoveWindow (-10,0)))
         , ((mod1Mask .|. controlMask .|. shiftMask, xK_k), withFocused (keysResizeWindow (0,-10) (0, 0)))
         , ((mod1Mask .|. controlMask .|. shiftMask, xK_j), withFocused (keysResizeWindow (0,10) (0, 0)))
         , ((mod1Mask .|. controlMask .|. shiftMask, xK_l), withFocused (keysResizeWindow (10,0) (0, 0)))
         , ((mod1Mask .|. controlMask .|. shiftMask, xK_h), withFocused (keysResizeWindow (-10,0) (0, 0)))


{-
         , ((mod1Mask, xK_l), focusUp)
         , ((mod1Mask, xK_h), focusDown)
         , ((mod1Mask .|. shiftMask , xK_l), swapUp)
         , ((mod1Mask .|. shiftMask , xK_h), swapDown)

         , ((mod1Mask, xK_comma), increaseNMasterGroups)
         , ((mod1Mask, xK_period), decreaseNMasterGroups)

         , ((mod1Mask .|. shiftMask, xK_comma), shrinkMasterGroups)
         , ((mod1Mask .|. shiftMask , xK_period), expandMasterGroups)

         , ((mod1Mask .|. controlMask, xK_k), moveToGroupUp False)
         , ((mod1Mask .|. controlMask, xK_j), moveToGroupDown False)

         , ((mod1Mask, xK_k), focusGroupUp)
         , ((mod1Mask, xK_j), focusGroupDown)
         , ((mod1Mask .|. shiftMask , xK_k), swapGroupUp)
         , ((mod1Mask .|. shiftMask , xK_j), swapGroupDown)

         , ((mod1Mask, xK_n), moveToNewGroupUp)
-}

         , ((mod1Mask, xK_j), bindOn LD [("ReflectX IM ReflectX Full", windows W.focusDown), ("", windowGo D True)])
         , ((mod1Mask, xK_k), bindOn LD [("ReflectX IM ReflectX Full", windows W.focusUp), ("", windowGo U True)])
         , ((mod1Mask, xK_l), windowGo R True)
         , ((mod1Mask, xK_h), windowGo L True)
         , ((mod1Mask .|. shiftMask, xK_j), windowSwap D True)
         , ((mod1Mask .|. shiftMask, xK_k), windowSwap U True)
         , ((mod1Mask .|. shiftMask, xK_l), windowSwap R True)
         , ((mod1Mask .|. shiftMask, xK_h), windowSwap L True)

         , ((mod1Mask .|. controlMask, xK_k), sendMessage $ pullGroup U)
         , ((mod1Mask .|. controlMask, xK_j), sendMessage $ pullGroup D)
         , ((mod1Mask .|. controlMask, xK_l), sendMessage $ pullGroup R)
         , ((mod1Mask .|. controlMask, xK_h), sendMessage $ pullGroup L)
         , ((mod1Mask .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))

         , ((mod1Mask, xK_Tab), bindOn LD [("ReflectX IM ReflectX Full", windows W.focusDown), ("", onGroup W.focusDown')])
         , ((mod1Mask .|. shiftMask, xK_Tab), bindOn LD [("ReflectX IM ReflectX Full", windows W.focusUp), ("", onGroup W.focusUp')])

         , ((mod1Mask, xK_period), sendMessage Expand)
         , ((mod1Mask, xK_comma), sendMessage Shrink)

         , ((mod1Mask .|. shiftMask, xK_period), sendMessage (IncMasterN (-1)))
         , ((mod1Mask .|. shiftMask, xK_comma), sendMessage (IncMasterN 1))

         ]
         ++
         [ ((mod1Mask, key), windows $ W.greedyView ws) | (ws, key) <- myExtraWS ]
         ++
         [ ((mod1Mask .|. shiftMask, key), windows $ W.shift ws) | (ws, key) <- myExtraWS ]
  where
    notNSPnonEmpty = WSIs $ return (\W.Workspace{..} -> tag /= "NSP" && isJust stack)


swapUp', swapDown' :: W.Stack a -> W.Stack a
swapUp'  (W.Stack t (l:ls) rs) = W.Stack t ls (l:rs)
swapUp'  (W.Stack t []     rs) = W.Stack t (reverse rs) []

reverseStack :: W.Stack a -> W.Stack a
reverseStack (W.Stack t ls rs) = W.Stack t rs ls
swapDown' = reverseStack . swapUp' . reverseStack

myWideFont  = "xft:JetBrainsMono Nerd Font Mono:style=Bold:size=30"
myShowWNameTheme = def
    { swn_fade              = 0.2
    , swn_font              = myWideFont
    , swn_bgcolor           = "#000000"
    , swn_color             = "#FFFFFF"
    }

myLayout =
    showWName' myShowWNameTheme $
    lessBorders OnlyScreenFloat $ avoidStruts $
    --spacingRaw True myBorder True myBorderIn True $
    --onWorkspace "0" Grid $
    reflectHoriz $ withIM (83%256) (ClassName "TelegramDesktop") $ reflectHoriz $
    trackFloating
    (
    --(windowNavigation $ mouseResizableTile) |||
      Full ||| (windowNavigation $ subTabbed $ sTall))
    where
      sTall = Tall 1 (2/100) (1/2)
      myBorder = Border 1 1 1 1
      myBorderIn = Border 8 8 8 8

-- use xprop to get these properties
myApps = composeAll $
    [ isFullscreen                                            --> doFullFloat
    , className =? "TelegramDesktop" <&&> title =? "Media viewer" --> doFloat
    ,  windowRole =? "pop-up"                                 --> doCenterFloat
    ,  windowType =? "_NET_WM_WINDOW_TYPE_DIALOG"             --> doCenterFloat
    ]
    ++ [ title =? t     --> doCenterFloat   | t <- centerFloatTitles ]
    ++ [ className =? n --> doCenterFloat   | n <- centerFloatClassNames ]
    ++ [ className =? n --> floatUnderMouse | n <- mouseFloatClassNames ]
    ++ [ className =? n --> doFullFloat | n <- fullFloatClassNames ]
  where
    windowRole = stringProperty "WM_WINDOW_ROLE"
    windowType = stringProperty "_NET_WM_WINDOW_TYPE"
    floatUnderMouse =
      placeHook (withGaps (25,0,0,0) $ underMouse (0.2, 0)) <+> doFloat
    centerFloatTitles =
      [ "Open File"
      , "File Operation Progress"
      , "Task Manager - Google Chrome"
      , "Volume Control"
      , "Friends List"
      , "Steam - News"
      ]
    centerFloatClassNames =
      [ "Wpg"
      , "Clockify"
      , "mpv"
      , "Zenity"
      , "Yad"
      , "Screenshot Monitor"
      ]
    mouseFloatClassNames =
      [ "MEGAsync"
--      , "Toggl Desktop"
      ]
    fullFloatClassNames =
      [ "Conky"
      ]

myScratchpads =
  [ -- Habitica (created with brave)
    NS "Habitica"
       "gtk-launch brave-pdigihnmoiplkhocekidmdcmhchhdpjo-Default.desktop"
       (title =? "Habitica - Gamify Your Life")
       defaultFloating

  ,  NS "RememberTheMilk"
       "gtk-launch brave-hbibbhnebobgojikephdhjmdokgkckna-Default.desktop"
       (title >>= \s -> return ("Remember The Milk" `isSuffixOf` s))
       defaultFloating

  , NS "Conky"
       "conky"
       (className =?  "Conky")
       defaultFloating
  ]

-- dynStatusBarEventHook expects a `ScreenId -> IO Handle`
barInScreen :: ScreenId -> IO Handle
barInScreen (S 0) = do
    spawn "sleep 1; polybar bar1"
    spawn "sleep 3; tint2 -c /home/augusto/.config/tint2/leftbar.tint2rc"
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
    --
    --
    --

myExtraWS = [("0", xK_0),("-", xK_minus),("=", xK_equal)]

myWorkspaces :: [String]
myWorkspaces = [show i | i <- [1..9]] ++ map fst myExtraWS


main = do
    --spawn "tint2 -c /home/augusto/.config/tint2/empty-background.tint2rc"
    --spawn "sleep 1 && tint2 -c /home/augusto/.config/tint2/workspaces.tint2rc"
    startUpBeep
    xmonad $ ewmh def
      { manageHook = insertPosition Below Newer <+> myApps <+> manageDocks <+> manageHook def
      , layoutHook = myLayout
      , handleEventHook = docksEventHook
                      -- <+> focusOnMouseMove
                       <+> dynStatusBarEventHook barInScreen (spawn "killall polybar; killall tint2")
                       <+> handleEventHook def
      , modMask = mod1Mask
      , workspaces = myWorkspaces
      , terminal  = "kitty"
      , focusedBorderColor = color6
      , normalBorderColor = color0
      , borderWidth = 2
      , startupHook = setWMName "LG3D" -- TODO what is this?
                    <+> docksStartupHook
                     <+> dynStatusBarStartup barInScreen (return ())
                    -- <+> adjustEventInput
       , logHook = myLogHook
      } `additionalKeys` myKeys
