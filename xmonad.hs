import System.IO
import System.Exit

import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers(doFullFloat, doCenterFloat, isFullscreen, isDialog)
import XMonad.Config.Desktop
import XMonad.Config.Azerty
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Actions.CycleWS
import XMonad.Hooks.UrgencyHook
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
---import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.Cross(simpleCross)
import XMonad.Layout.Spiral(spiral)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Grid
import XMonad.Layout.BinarySpacePartition
import XMonad.Actions.MouseResize
import XMonad.Layout.WindowArranger
import Data.Monoid
import XMonad.Hooks.ManageHelpers



import XMonad.Layout.CenteredMaster(centerMaster)

import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.ByteString as B
import Control.Monad (liftM2)
import qualified DBus as D
import qualified DBus.Client as D



import XMonad.Actions.TagWindows
import XMonad.Prompt    -- to use tagPrompt

import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S

import XMonad.Layout.Maximize 
import XMonad.Layout.Minimize
import XMonad.Hooks.Minimize 
import XMonad.Actions.Minimize (minimizeWindow, withLastMinimized, maximizeWindowAndFocus, withMinimized, maximizeWindow)
import XMonad.Actions.WindowBringer (windowMap, bringMenu, bringMenu', actionMenu)
import XMonad.Util.Dmenu
import XMonad.StackSet (focusWindow)
import XMonad.Actions.WindowBringer (bringMenuArgs,bringMenuArgs', gotoMenuArgs',bringWindow)
import qualified XMonad.Util.Dmenu as Dmenu
import qualified Data.Maybe as Maybe
import XMonad.Actions.TagWindows (addTag, hasTag, delTag, withTaggedGlobalP)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Layout.Hidden
import qualified Rofi
import XMonad.Util.NamedScratchpad
import XMonad.ManageHook




myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"

-- colours
normBord = "#4c566a"
focdBord = "#5e81ac"
fore     = "#DEE3E0"
back     = "#282c34"
winType  = "#c678dd"

fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#504945"
bg3       = "#665c54"
bg4       = "#7c6f64"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"

--mod4Mask= super key
--mod1Mask= alt key
--controlMask= ctrl key
--shiftMask= shift key

myModMask = mod4Mask
encodeCChar = map fromIntegral . B.unpack
myFocusFollowsMouse = False
myBorderWidth = 1
myWorkspaces    = ["\61612","\61899","\61947","\61635","\61502","\61501","\61705","\61564","\62150","\61872"]
--myWorkspaces    = ["1","2","3","4","5","6","7","8","9","10"]
--myWorkspaces    = ["I","II","III","IV","V","VI","VII","VIII","IX","X"]

myBaseConfig = desktopConfig


-- window manipulations
myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    , [className =? "xfce4-appfinder" --> doCenterFloat]
    ]
    where
    -- doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Arandr", "Arcolinux-tweak-tool.py", "Arcolinux-welcome-app.py", "Galculator", "feh", "mpv", "Xfce4-terminal"]
    myTFloats = ["Downloads", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window"]




-- myLayout =  spacingRaw True (Border 0 1 1 1) True (Border 1 1 1 1) True $ minimize $ maximize $ avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ tiled ||| Mirror tiled ||| spiral (6/7)  ||| ThreeColMid 1 (3/100) (1/2) ||| Full
myLayout =  spacingRaw True (Border 0 1 1 1) True (Border 1 1 1 1) True $ minimize $ maximize $ avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ tiled ||| Grid ||| emptyBSP  ||| ThreeColMid 1 (3/100) (1/2) ||| Full
    where
        tiled = Tall nmaster delta tiled_ratio
        nmaster = 1
        delta = 3/100
        tiled_ratio = 1/2


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.empty

myLauncher = "rofi"

minimizedWindows = withMinimized return

minimizeFocused :: Window -> X ()
minimizeFocused w = do
  withFocused minimizeWindow

promptRestoreWindow = do
      wm <- windowMap
      shownWindows <- withMinimized (\minimizedWindows -> pure $ M.filter (`elem` minimizedWindows) wm)
      win <- menuArgs "rofi" ["-dmenu", "-i", "-show", "combi"] (M.keys shownWindows)
      whenJust (M.lookup win wm) (\w -> maximizeWindow w >> (windows $ bringWindow w))




fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f
rectFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery (doRectFloat $ W.RationalRect 0.05 0.05 0.9 0.9) f
scratchpads = [
-- run htop in xterm, find it by title, use default floating window placement
  NS "htop" "termite -e htop" (title =? "htop") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))


  , NS "scratchTerminal" "termite --title 'scratchTerminal'" (title =? "scratchTerminal") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    -- NS "htop" "termite -e htop" (title =? "htop") defaultFloating


-- run gvim, find by role, don't float
    -- . NS "notes" "gvim --role notes ~/notes.txt" (role =? "notes") nonFloating
    ] where role = stringProperty "WM_WINDOW_ROLE"



-- keys config

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- SUPER + FUNCTION KEYS

  [ ((modMask, xK_e), spawn $ "code" )
  , ((modMask, xK_w), spawn $ "google-chrome-stable" )
  , ((modMask, xK_c), spawn $ "conky-toggle" )
  , ((modMask, xK_f), sendMessage $ Toggle NBFULL)
  , ((modMask .|. mod1Mask, xK_h), namedScratchpadAction scratchpads "htop" )
  , ((modMask .|. mod1Mask, xK_t), namedScratchpadAction scratchpads "scratchTerm" )
  , ((modMask, xK_q), kill )
  , ((modMask, xK_y), spawn $ "polybar-msg cmd toggle" )
  , ((modMask, xK_x), spawn $ "arcolinux-logout" )
  , ((modMask, xK_Escape), spawn $ "xkill" )
  , ((modMask, xK_Return), spawn $ "termite" )
  -- , ((modMask , xK_s ), spawn $ "xfce4-appfinder")


  -- FUNCTION KEYS
  , ((0, xK_F12), spawn $ "xfce4-terminal --drop-down" )

  -- SUPER + SHIFT KEYS

  , ((modMask .|. shiftMask , xK_Return ), spawn $ "thunar")
  , ((modMask .|. shiftMask , xK_d ), spawn $ "dmenu_run -i -nb '#191919' -nf '#79d4ed' -sb '#fea63c' -sf '#79d4ed' -fn 'NotoMonoRegular:bold:pixelsize=14'")
  , ((modMask .|. shiftMask , xK_r ), spawn $ "xmonad --recompile && xmonad --restart")
  , ((modMask .|. shiftMask , xK_s ), spawn $ "rofi -combi-modi window,drun -show combi -modi combi")

  -- CONTROL + ALT KEYS

  -- ALT + ... KEYS

  , ((modMask .|. mod1Mask, xK_f), spawn $ "variety -f" )
  , ((modMask .|. mod1Mask, xK_n), spawn $ "variety -n" )
  , ((modMask .|. mod1Mask, xK_p), spawn $ "variety -p" )
  , ((modMask .|. mod1Mask, xK_t), spawn $ "variety -t" )
  , ((modMask .|. mod1Mask, xK_Left), spawn $ "variety -p" )
  , ((modMask .|. mod1Mask, xK_Right), spawn $ "variety -n" )

  --CONTROL + SHIFT KEYS


  --SCREENSHOTS

  , ((0, xK_Print), spawn $ "scrot 'ArcoLinux-%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES)'")
  , ((controlMask, xK_Print), spawn $ "xfce4-screenshooter" )
  , ((controlMask .|. shiftMask , xK_Print ), spawn $ "gnome-screenshot -i")


  --MULTIMEDIA KEYS

  -- Mute volume
  , ((0, xF86XK_AudioMute), spawn $ "amixer -q set Master toggle")

  -- Decrease volume
  , ((0, xF86XK_AudioLowerVolume), spawn $ "amixer -q set Master 5%-")

  -- Increase volume
  , ((0, xF86XK_AudioRaiseVolume), spawn $ "amixer -q set Master 5%+")

  -- Increase brightness
  , ((0, xF86XK_MonBrightnessUp),  spawn $ "xbacklight -inc 10")

  -- Decrease brightness
  , ((0, xF86XK_MonBrightnessDown), spawn $ "xbacklight -dec 10")

  , ((0, xF86XK_AudioPlay), spawn $ "playerctl play-pause")
  , ((0, xF86XK_AudioNext), spawn $ "playerctl next")
  , ((0, xF86XK_AudioPrev), spawn $ "playerctl previous")
  , ((0, xF86XK_AudioStop), spawn $ "playerctl stop")


  --------------------------------------------------------------------
  --  XMONAD LAYOUT KEYS

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space), sendMessage NextLayout)


  --Focus selected desktop
  , ((modMask, xK_Tab), nextWS)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

  -- Move focus to the next window.
  , ((modMask, xK_Right), windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_Left), windows W.focusUp  )

  -- Move focus to the master window.
  -- , ((modMask .|. shiftMask, xK_m), windows W.focusMaster  )

  -- Swap the focused window with the next window.
--   , ((modMask .|. shiftMask, xK_j), windows W.swapDown  )

  -- Swap the focused window with the next window.
  -- , ((controlMask .|. modMask, xK_Down), windows W.swapDown  )

  -- Swap the focused window with the previous window.
--   , ((modMask .|. shiftMask, xK_k), windows W.swapUp    )

  -- Swap the focused window with the previous window.
  , ((shiftMask .|. modMask, xK_Left), windows W.swapUp  )
  
  , ((shiftMask .|. modMask, xK_Right), windows W.swapDown  )

  -- Shrink the master area.
  , ((controlMask .|. modMask , xK_Left), sendMessage Shrink)

  -- Expand the master area.
  , ((controlMask .|. modMask , xK_Right), sendMessage Expand)


  , ((modMask, xK_m), withFocused minimizeWindow)
  
  -- , ((modMask .|. shiftMask, xK_m), bringRestoredWindow ) 
  , ((modMask .|. shiftMask, xK_m), promptRestoreWindow ) 


	, ((modMask, xK_t), rectFloatFocused )
	, ((modMask .|. shiftMask, xK_t),  withFocused $ windows . W.sink )
  , ((modMask, xK_f), fullFloatFocused) 
  , ((modMask .|. shiftMask, xK_f), withFocused $ windows . W.sink) 


  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)

  --Keyboard layouts
  --qwerty users use this line
   | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9,xK_0]

  --French Azerty users use this line
  -- | (i, k) <- zip (XMonad.workspaces conf) [xK_ampersand, xK_eacute, xK_quotedbl, xK_apostrophe, xK_parenleft, xK_minus, xK_egrave, xK_underscore, xK_ccedilla , xK_agrave]

  --Belgian Azerty users use this line
  -- | (i, k) <- zip (XMonad.workspaces conf) [xK_ampersand, xK_eacute, xK_quotedbl, xK_apostrophe, xK_parenleft, xK_section, xK_egrave, xK_exclam, xK_ccedilla, xK_agrave]

      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)
      , (\i -> W.greedyView i . W.shift i, shiftMask)]]

  -- ++
  -- -- ctrl-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- -- ctrl-shift-{w,e,r}, Move client to screen 1, 2, or 3
  -- [((m .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  --     | (key, sc) <- zip [xK_w, xK_e] [0..]
  --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myHandleEventHook = minimizeEventHook


main :: IO ()
main = do

    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]


    xmonad . ewmh $
  --Keyboard layouts
  --qwerty users use this line
            myBaseConfig
  --French Azerty users use this line
            --myBaseConfig { keys = azertyKeys <+> keys azertyConfig }
  --Belgian Azerty users use this line
            --myBaseConfig { keys = belgianKeys <+> keys belgianConfig }

                {startupHook = myStartupHook
, layoutHook = gaps [(U,35), (D,1), (R,1), (L,1)] $ mouseResize $ windowArrange $ myLayout ||| layoutHook myBaseConfig
, manageHook = namedScratchpadManageHook scratchpads <+> manageSpawn <+> myManageHook <+> manageHook myBaseConfig
, modMask = myModMask
, borderWidth = myBorderWidth
, handleEventHook    = handleEventHook myBaseConfig <+> fullscreenEventHook <+> myHandleEventHook
, focusFollowsMouse = myFocusFollowsMouse
, workspaces = myWorkspaces
, focusedBorderColor = focdBord
, normalBorderColor = normBord
, keys = myKeys
, mouseBindings = myMouseBindings

}

