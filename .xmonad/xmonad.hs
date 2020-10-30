import XMonad

import Control.Monad

import System.Exit
import Graphics.X11.ExtraTypes.XF86

import XMonad.Actions.SinkAll
import XMonad.Actions.WindowBringer


import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Accordion  
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane (TwoPane(..))
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.NoBorders
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Util.WorkspaceCompare

import XMonad.Prompt
import XMonad.Prompt.Window
import XMonad.Prompt.Layout

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myTerminal = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth = 1

myModMask = mod4Mask

myWorkspaces = withScreens 2 $ map show [1..9]

myNormalBorderColor  = "#aaaaaa"
myFocusedBorderColor = "#ff0000"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)

    -- volume keys
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")

    -- launch rofi
    , ((modm,               xK_d     ), spawn "rofi -show run")

    -- close focused window
    , ((modm .|. shiftMask, xK_x     ), kill)
    , ((modm, xK_x     ), sinkAll)

    , ((modm, xK_f ), sendMessage $ Toggle FULL)
    
      -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    , ((modm,               xK_g     ), gotoMenu)
    
    , ((modm,               xK_p     ), layoutPrompt def)
    
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_k     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_j     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    
    -- Swap the focused window and the master window
    , ((modm .|. shiftMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm .|. shiftMask, xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm .|. shiftMask, xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_0     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. shiftMask, xK_r     ), spawn "xmonad --recompile && xmonad --restart")
    
    -- Power menu, lock screen, etc.
    , ((modm .|. shiftMask, xK_p     ), spawn "~/.local/bin/rofi-power")
    
    -- Lock screen
    , ((modm, xK_l                   ), spawn "i3lock-fancy")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{q,w}, Switch to physical/Xinerama screens 1 or 2
    -- mod-shift-{q,w}, Move client to screen 1 or 2
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_q, xK_w] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myTabConfig = def {
        activeColor = "#121212",
        inactiveColor = "#000000",
        urgentColor = "#FDF6E3",
        activeBorderWidth = 0,
        inactiveBorderWidth = 0,
        urgentBorderWidth = 0,
        activeTextColor = "#FFFFFF",
        inactiveTextColor = "#999999",
        urgentTextColor = "#1ABC9C",
        fontName = "xft:JetBrains Mono:size=7:antialias=true:autohint=true"
}

myLayout = avoidStruts
           $ noBorders (tabbed shrinkText myTabConfig)
           ||| Accordion
	   ||| Grid
           ||| TwoPane (15/100) (55/100)
           ||| noBorders Full
           ||| tiled
           ||| Mirror tiled
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myManageHook = composeAll 
    [ className =? "Emacs"          --> doShiftAndGo "0_2"
    , className =? "qutebrowser"    --> doShiftAndGo "0_3"
    , className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]
    where
      doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift


myEventHook = mempty

myLogHook = return ()

myStartupHook = return ()

myBar = "xmobar"

myPP = xmobarPP {
     ppCurrent          = xmobarColor "#FFFFFF" "" . wrap ">" "<",
     ppVisible          = xmobarColor "#888888" "" . wrap "<" ">",
     ppVisibleNoWindows = Just $ mempty . wrap "<" ">",
     ppHidden           = xmobarColor "#888888" "" . wrap "<" ">",
     -- ppHiddenNoWindows  = xmobarColor "#999999" "" . wrap "" "",
     ppUrgent           = xmobarColor "#ff0000" "" . wrap "<" ">",
     ppSep              = " ",
     ppTitle            = xmobarColor "#999999" "",
     ppLayout           = xmobarColor "#555555" "" . (\l -> case l of
          "Tabbed Simplest" -> "[_]"
          "Tabbed"          -> "[_]"
          "Accordion"       -> "[A]"
	  "Grid"            -> "[+]"
          "TwoPane"         -> "[|]"
          "Full"            -> "[O]"
          "Tall"            -> "[T]"
          "Mirror Tall"     -> "[-]"
          l -> l ),
     ppSort             = getSortByTag,
     ppExtras           = [] }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main = xmonad =<< statusBar myBar myPP toggleStrutsKey defaults

defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
