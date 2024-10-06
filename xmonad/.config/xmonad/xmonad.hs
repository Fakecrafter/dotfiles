{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
--IMPORTS

import XMonad
import XMonad.Config
import Data.Monoid
import System.Exit

--LAYOUTS
-- import XMonad.Layout.MultiColumns
-- import XMonad.Layout.MagicFocus
-- import XMonad.Layout.Renamed
import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed

--UTILS
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

--ACTIONS
import XMonad.Actions.CopyWindow
import XMonad.Actions.Submap
import XMonad.Actions.CycleWS

--HOOKS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.DynamicLog

--OTHER
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


main = do
    xmproc <- spawnPipe "dbus-launch xmobar -x 0 /home/fakecrafter/.config/xmobar/xmobarrc.hs"
    xmonad $ docks def {
      -- simple stuff
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook <+> namedScratchpadManageHook scratchpads,
        handleEventHook    = myEventHook,
        logHook            = dynamicLogWithPP $
                    xmobarPP {
                              ppCurrent = xmobarColor "#d65d0e" "" . wrap "[" "]"
                            , ppHidden = xmobarColor "#fe8019" ""
                            , ppHiddenNoWindows = xmobarColor "#a89984" ""
                            , ppLayout = xmobarColor "#a89984" ""
                            , ppTitle = const ""
                            , ppSep = " - "
                            , ppOutput = hPutStrLn xmproc
                            },
        startupHook        = myStartupHook
    }




-- VARIABLES

myTerminal      = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = True

myBorderWidth   = 2

myModMask       = mod4Mask


myWorkspaces    = ["DEV","WEB","GEN","SYS","GFX"]

myNormalBorderColor  = "#928374"
myFocusedBorderColor = "#fb4934"

myFont = "Iosevka Nerd Font"


-- COLORS

active = "#fb4934"
unactive = "#928374"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.

myKeys = \c -> mkKeymap c $

    [ ("M-<Return>", spawn myTerminal)

--ROFI and PROMPTS
    -- launch rofi
    , ("M-<Space>", spawn "rofi -show run")
--    , ("M-c", spawn "rofi -show calc")
    , ("M-x", spawn "rofi -show power-menu -modi power-menu:rofi-power-menu")
    , ("M-s", spawn "/home/fakecrafter/bin/rofi-screenshot")
--    , ("M-e", spawn "rofi -show emoji")
    , ("M-g", spawn "/home/fakecrafter/bin/rofo-pass")

    -- launch emacsclient
    , ("M-e", spawn "emacsclient -c")
    -- launch firefox
    , ("M-w", spawn "firefox")
    -- play-pause music
    , ("M-p", spawn "playerctl --player=cmus play-pause")


    -- close focused window
    , ("M-q", sendMessage ToggleStruts)

     -- Rotate through the available layout algorithms
    , ("M-<Tab>", sendMessage NextLayout)

    , ("M-v", windows copyToAll)
    , ("M-S-v", killAllOtherCopies)
    --  Reset the layouts on the current workspace to default
    , ("M-S-<Space>", setLayout $ XMonad.layoutHook c)

    -- Resize viewed windows to the correct size



--SCRATCHPADS

    , ("M-i M-d", namedScratchpadAction scratchpads "term")
    , ("M-i M-f", namedScratchpadAction scratchpads "vifm")
    , ("M-i M-p", namedScratchpadAction scratchpads "pavucontrol")
    , ("M-i S-p", namedScratchpadAction scratchpads "periodensystem")
    , ("M-i M-m", namedScratchpadAction scratchpads "music")

    -- Move focus to the next window
    , ("M-j", windows W.focusDown)

    -- Move focus to the previous window
    , ("M-k", windows W.focusUp  )

    -- Move focus to the master window
    , ("M-n", windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ("M-m", windows W.swapMaster)

    -- Swap the focused window with the next window
    , ("M-S-j", windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ("M-S-k", windows W.swapUp    )

    -- Shrink the master area
    , ("M-h", sendMessage Shrink)

    -- Expand the master area
    , ("M-l", sendMessage Expand)

    -- Push window back into tiling
    , ("M-t", withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ("M-,", prevWS)

    , ("M-.", nextWS)
    -- Deincrement the number of windows in the master area
    , ("M-;", sendMessage (IncMasterN (-1)))

    , ("M-<Backspace>", kill)

    -- Quit xmonad
    , ("M-S-q", io (exitWith ExitSuccess))

    -- Restart xmonad
    , ("M-S-r", spawn "killall xmobar;xmonad --recompile; xmonad --restart")
    ]


    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    ++
    [(m ++ k, windows $ f w)
        | (w, k) <- zip (XMonad.workspaces c) (map show [1..9])
        , (m, f) <- [("M-",W.view), ("M-S-",W.shift)]] -- was W.greedyView

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

------------------------------------------------------------------------
-- Scratchpads
------------------------------------------------------------------------

scratchpads :: [NamedScratchpad]
scratchpads = [
    NS "term" "alacritty -t scratchpad" (title =? "scratchpad")
        (customFloating $ W.RationalRect (0/1) (2/3) (1/1) (1/3)),
    NS "vifm" "alacritty -t vifm -e vifm" (title =? "vifm")
        (customFloating $ W.RationalRect (1/6) (1/6) (4/6) (4/6)),
    NS "pavucontrol" "alacritty -t pulsemixer -e pulsemixer" (title =? "pulsemixer")
        (customFloating $ W.RationalRect (2/4) (1/4) (2/4) (2/4)),
    NS "periodensystem" "sxiv ~/Bilder/Periodensystem.png" (title =? "sxiv")
        (customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4)),
    NS "music" "alacritty -t music -e cmus" (title =? "music")
        (customFloating $ W.RationalRect (0/4) (1/4) (1/2) (1/2))
  ]

------------------------------------------------------------------------
-- Layouts:

myLayout = avoidStruts $ (full ||| simpleTabbed ||| tall)
  where
    full = noBorders $ Full
    tall   = Tall masterwindows delta ratio
    masterwindows = 1
    ratio   = 0.55
    delta   = 4/100


------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , title =? "Mozilla Firefox"     --> doShift ( myWorkspaces !! 1 )
    , className =? "lightcord"        --> doShift ( myWorkspaces !! 3 )
    , className =? "discord"        --> doShift ( myWorkspaces !! 3 )
    , className =? "element"        --> doShift ( myWorkspaces !! 3 )
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
myStartupHook = do
  spawn "feh --randomize --bg-fill $HOME/Bilder/Hintergrundbilder/gute/*"
