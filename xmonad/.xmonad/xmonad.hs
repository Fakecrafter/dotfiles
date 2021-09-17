--IMPORTS

import XMonad
import Data.Monoid
import System.Exit

--PROMPTS
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.Unicode
import XMonad.Prompt.XMonad
import Control.Arrow (first)

--LAYOUTS
import XMonad.Layout.MultiColumns
import XMonad.Layout.MagicFocus
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders


--UTILS
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad


--ACTIONS
import XMonad.Actions.CopyWindow
import XMonad.Actions.Submap


--HOOKS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.DynamicLog


--OTHER
import qualified XMonad.StackSet as W
import qualified Data.Map        as M








-- VARIABLES

myTerminal      = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = True

-- Width of the window border in pixels.
--
myBorderWidth   = 4

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask


--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["dev","www","sys","chat","vid","gfx","data","other"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#928374"
myFocusedBorderColor = "#fb4934"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
-- myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
myKeys = \c -> mkKeymap c $

    [ ("M-<Return>", spawn $ XMonad.terminal c)

--ROFI and PROMPTS
    -- launch rofi
    , ("M-<Space>", spawn "rofi -show run")
    , ("M-c", spawn "rofi -show calc")
    , ("M-b", spawn "buku_run")
    , ("M-x", spawn "rofi -show power-menu -modi power-menu:rofi-power-menu")
    , ("M-s", spawn "rofi-screenshot")
    , ("M-e", spawn "rofi -show emoji")
    , ("M-g", spawn "~/scripts/rofo-pass")

    -- launch firefox
    , ("M-w", spawn "firefox")
    -- play-pause music
    , ("M-p s", spawn "playerctl --player=cmus play-pause; playerctl --player=spotify play-pause")
    , ("M-p f", spawn "playerctl --player=firefox.instance3122 play-pause")


    -- close focused window
    , ("M-q", kill)

    , ("M-z", spawn "echo hello")
     -- Rotate through the available layout algorithms
    , ("M-<Tab>", sendMessage NextLayout)

    , ("M-v", windows copyToAll)
    , ("M-S-v", killAllOtherCopies)
    --  Reset the layouts on the current workspace to default
    , ("M-S-<Space>", setLayout $ XMonad.layoutHook c)

    -- Resize viewed windows to the correct size
    , ("M-n", refresh)


    -- Move focus to the next window
    --, ((modm,               xK_Tab   ), windows W.focusDown)

--SCRATCHPADS

    , ("M-i d", namedScratchpadAction scratchpads "term")
    , ("M-i f", namedScratchpadAction scratchpads "vifm")
    , ("M-i p", namedScratchpadAction scratchpads "pavucontrol")
    , ("M-i S-p", namedScratchpadAction scratchpads "periodensystem")
    , ("M-i c", namedScratchpadAction scratchpads "music")

    -- Move focus to the next window
    , ("M-j", windows W.focusDown)

    -- Move focus to the previous window
    , ("M-k", windows W.focusUp  )

    -- Move focus to the master window
    , ("M-m", windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ("M-S-<Return>", windows W.swapMaster)

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
    , ("M-,", sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ("M-;", sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

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

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Scratchpads
------------------------------------------------------------------------

scratchpads :: [NamedScratchpad]
scratchpads = [
-- run htop in alacritty, find it by title, use default floating window placement
    NS "term" "alacritty -t scratchpad -o background_opacity=0.8" (title =? "scratchpad")
        (customFloating $ W.RationalRect (0/1) (2/3) (1/1) (1/3)),
    NS "vifm" "alacritty -t vifm -o background_opacity=0.8 -e vifm" (title =? "vifm")
        (customFloating $ W.RationalRect (1/6) (1/6) (4/6) (4/6)),
    NS "pavucontrol" "alacritty -t pulsemixer -o background_opacity=0.8 -e pulsemixer" (title =? "pulsemixer")
        (customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4)),
    NS "periodensystem" "sxiv ~/Bilder/Periodensystem.png" (title =? "sxiv")
        (customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4)),
    NS "music" "alacritty -t music -o background_opacity=0.8 -e cmus" (title =? "music")
        (customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2))
  ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts (Full ||| magicFocus (tiled) ||| tiled ||| Mirror tiled)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 70/100

     -- Percent of screen to increment by when resizing panes
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
    , title =? "Hinzufügen"           --> doFloat
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
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do 
  spawn "feh --randomize --bg-fill $HOME/Bilder/Hintergrundbilder/gute/*"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    xmproc <- spawnPipe "xmobar -x 0 /home/fakecrafter/.config/xmobar/xmobarrc.hs"
    xmonad $ docks defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
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
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = insertPosition End Newer <+> myManageHook <+> namedScratchpadManageHook scratchpads,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

