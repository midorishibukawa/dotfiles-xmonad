-- base
import XMonad
import System.Exit (exitSuccess)
import System.IO
import qualified XMonad.StackSet as W

-- data
import Data.Monoid
import Data.Ratio
import qualified Data.Map as M

-- hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (doFullFloat, doRectFloat)
import XMonad.Hooks.ServerMode

-- actions
import XMonad.Actions.Navigation2D

-- layouts
import XMonad.Layout.BinarySpacePartition

-- layout modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing

-- utilities
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)

-- set modkey to super
myModMask   :: KeyMask
myModMask   = mod4Mask

-- default apps
myEditor    :: String
myEditor    = "nvim"

myTerminal  :: String
myTerminal  = "kitty"

-- get the active workspace tag
getActiveWorkspace :: X String
getActiveWorkspace = do
    workspaces <- gets windowset
    return $ W.tag . W.workspace . W.current $ workspaces

-- 

-- keybinds
myKeys conf = M.fromList $ []

-- set layouts and stuff
    -- set themes
myBorderWidth :: Dimension
myBorderWidth = 0

myFont :: String
myFont = "xft:Source Sans Pro:regular:size=10:antialias=true:hinting=true"

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i 0 i 0) True (Border 0 i 0 i) True

myAltSpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
myAltSpacing i = spacingRaw False (Border i i i i) True (Border 0 0 0 0) True

    -- set hooks
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll . concat $
    [ [className     =? w04 --> doShift     (myWorkspaces !!  4)    | w04   <- design]
    , [className     =? rfloat  --> doRectFloat (W.RationalRect (1 % 8) (1 % 8) (3 % 4) (3 % 4)) | rfloat <- float]
    , [title     =? "DISGAEA" --> doFloat ]
    ]
  where design      = ["Gimp-2.10", "krita", "Inkscape", "Kdenlive"]
        float       = ["glances", "feh", "DISGAEA"]

    -- commands to be used with xmonadctl and invoked by sxhkd
myCommands :: [(String, X ())]
myCommands =
        [ ("next-layout"                , sendMessage NextLayout)
        , ("decrease-master-size"       , sendMessage Shrink)
        , ("increase-master-size"       , sendMessage Expand)
        , ("expand-towards-up"          , sendMessage $ ExpandTowards U)
        , ("expand-towards-right"       , sendMessage $ ExpandTowards R)
        , ("expand-towards-down"        , sendMessage $ ExpandTowards D)
        , ("expand-towards-left"        , sendMessage $ ExpandTowards L)
        , ("shrink-from-up"             , sendMessage $ ShrinkFrom U)
        , ("shrink-from-right"          , sendMessage $ ShrinkFrom R)
        , ("shrink-from-down"           , sendMessage $ ShrinkFrom D)
        , ("shrink-from-left"           , sendMessage $ ShrinkFrom L)
        , ("rotate"                     , sendMessage Rotate)
        , ("swap"                       , sendMessage Swap)
        , ("focus-parent"               , sendMessage FocusParent)
        , ("select-node"                , sendMessage SelectNode)
        , ("move-node"                  , sendMessage MoveNode)
        , ("decrease-master-count"      , sendMessage $ IncMasterN (-1))
        , ("increase-master-count"      , sendMessage $ IncMasterN ( 1))
        , ("expand-up"                  , sendMessage $ ExpandTowards U)
        , ("expand-right"               , sendMessage $ ExpandTowards R)
        , ("expand-down"                , sendMessage $ ExpandTowards D)
        , ("expand-left"                , sendMessage $ ExpandTowards L)
        , ("focus-prev"                 , windows W.focusUp)
        , ("focus-next"                 , windows W.focusDown)
        , ("focus-master"               , windows W.focusMaster)
        , ("focus-up"                   , windowGo U True)
        , ("focus-right"                , windowGo R True)
        , ("focus-down"                 , windowGo D True)
        ,("focus-left"                 , windowGo L True)
        , ("swap-with-prev"             , windows W.swapUp)
        , ("swap-with-next"             , windows W.swapDown)
        , ("swap-with-master"           , windows W.swapMaster)
        , ("kill-window"                , kill)
        , ("quit"                       , io exitSuccess)
        , ("restart"                    , spawn "xmonad --recompile; xmonad --restart")
        ]

myServerModeEventHook = serverModeEventHookCmd' $ return myCommands'
myCommands' = ("list-commands", listMyServerCmds) : myCommands ++ wscs
  where
    wscs =  [ ((m ++ s), windows $f s) | s <- myWorkspaces
            , (f,m) <- [(W.view, "focus-workspace-"), (W.shift, "send-to-workspace-")]
            ]

listMyServerCmds :: X ()
listMyServerCmds = spawn ("echo '" ++ asmc ++ "' | xmessage -file -")
  where asmc = concat $ "available commands:" : map (\(x, _)-> "    " ++ x) myCommands'

    -- set workspaces
myWorkspaces       = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"]

current xs  = "<fn=1>\xF111</fn>"
visible xs  = "<fn=1>\xF111</fn>"
hidden  xs  = "<fn=1>\xF1DB</fn>"
blank   xs  = "<fn=0>\x2219</fn>"
alert   xs  = "<fn=1>\xF06A</fn>"

-- set layouts
myLayoutHook    = bsp ||| Full
  where{-
    tall        = avoidStruts
                $ mySpacing 4
                $ Tall 1 (3/100) (1/2)
                -}
    bsp         = avoidStruts
                $ mySpacing 4
                $ emptyBSP

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar $XDG_CONFIG_HOME/xmonad/xmobar.hs"
    xmonad $ ewmh . docks $ def
        { borderWidth       = myBorderWidth
        , handleEventHook   = myServerModeEventHook
        , keys              = myKeys
        , layoutHook        = myLayoutHook
        , logHook           = dynamicLogWithPP $ xmobarPP
            { ppOutput          = \x -> hPutStrLn xmproc x
            , ppTitle           = shorten 60
            , ppCurrent         = current
            , ppVisible         = visible
            , ppHidden          = hidden
            , ppHiddenNoWindows = blank
            , ppUrgent          = alert
            , ppSep             = " | "
            , ppExtras          = []
            , ppOrder           = \(ws:_:t:ex) -> [ws]++ex++[t]
            }
        , manageHook        = myManageHook <+> manageDocks
        , modMask           = myModMask
        , terminal          = myTerminal
        , workspaces        = myWorkspaces
        }
