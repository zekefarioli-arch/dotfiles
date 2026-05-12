-- ~/.xmonad/xmonad.hs
-- XMonad + Polybar DBus (Catppuccin Mocha)

import XMonad
import XMonad.Config (def)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)

import XMonad.ManageHook (doFloat, composeAll, (-->))

import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.StackSet as W

import System.Exit (exitWith, ExitCode(ExitSuccess))

import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders

import XMonad.Util.NamedWindows (getName)

-- ==========================================================================
-- F1 LAYOUT
-- ==========================================================================

data F1Layout a = F1Layout deriving (Show, Read)

instance LayoutClass F1Layout Window where
    doLayout F1Layout rect stack = do
        let ws = W.integrate stack
            Rectangle sx sy sw sh = rect

            cellW = sw `div` 3
            cellH = sh `div` 3

            r1 = Rectangle sx sy (2 * cellW) (2 * cellH)

            r2 = Rectangle (sx + fromIntegral (2 * cellW)) sy cellW cellH
            r3 = Rectangle (sx + fromIntegral (2 * cellW))
                           (sy + fromIntegral cellH)
                           cellW cellH

            r6 = Rectangle sx
                           (sy + fromIntegral (2 * cellH))
                           cellW cellH

            r5 = Rectangle (sx + fromIntegral cellW)
                           (sy + fromIntegral (2 * cellH))
                           cellW cellH

            r4 = Rectangle (sx + fromIntegral (2 * cellW))
                           (sy + fromIntegral (2 * cellH))
                           cellW cellH

            rects = [r1,r2,r3,r6,r5,r4]

        return (zip ws rects, Nothing)

    pureMessage _ _ = Nothing

-- ==========================================================================
-- COLORS (Catppuccin Mocha)
-- ==========================================================================

colorBack = "#1e1e2e"
colorAct  = "#f5c2e7"
colorVis  = "#89b4fa"
colorOcc  = "#313244"
colorEmp  = "#6c7086"

myBorderWidth = 2
myNormColor   = "#313244"
myFocusColor  = "#f5c2e7"

-- ==========================================================================
-- WORKSPACES
-- ==========================================================================

myWorkspaces =
    [ "1 \xF268"
    , "2 \xF07C"
    , "3 \xF120"
    , "4 \xF09B"
    , "5 \xE70C"
    , "6 \xF001"
    , "7 \xF02AB"
    , "8 \xF232"
    , "9 \xf1c2"
    ]

-- ==========================================================================
-- LAYOUTS
-- ==========================================================================

myLayout = avoidStruts $
    F1Layout ||| tiled ||| Mirror tiled ||| noBorders Full ||| Grid ||| threeCol
  where
    tiled    = Tall 1 (3/100) (1/2)
    threeCol = ThreeColMid 1 (3/100) (1/2)

-- ==========================================================================
-- CUSTOM LOGGERS (Polybar / DBus)
-- ==========================================================================

logScreen :: X (Maybe String)
logScreen = do
    s <- gets windowset
    return $ Just $ "SCREEN:" ++ show (fromIntegral (W.screen (W.current s)) :: Int)

logLayouts :: X (Maybe String)
logLayouts = do
    ws <- gets windowset
    let allScreens = W.current ws : W.visible ws
        formatScreen s = "LAY" ++ show (fromIntegral (W.screen s) :: Int) ++ ":" ++ description (W.layout (W.workspace s))
    return $ Just $ unwords $ map formatScreen allScreens

logWinTitles :: X (Maybe String)
logWinTitles = do
    ws <- gets windowset
    let allScreens = W.current ws : W.visible ws
    parts <- mapM one allScreens
    return $ Just $ unwords parts
  where
    one s = do
        let sid = show (fromIntegral (W.screen s) :: Int)
            mW  = W.focus <$> W.stack (W.workspace s)
        titleString <- case mW of
            Nothing -> return "-"
            Just w  -> do
                cls  <- runQuery className w
                name <- fmap show (getName w)
                return $ if cls == name || null name
                    then cls
                    else cls ++ " - " ++ name
        return $ "WIN" ++ sid ++ ":" ++ filter (/= '\n') titleString

wrapClick ws content =
    "%{A1:xdotool key super+" ++ ws ++ ":}" ++ content ++ "%{A}"

dbusPP dbus = def
    { ppOutput = \str -> do
        let signal = (D.signal objectPath interfaceName memberName)
                { D.signalBody = [D.toVariant str] }
        D.emit dbus signal
    , ppCurrent = \ws -> wrapClick ws $
        "%{B" ++ colorAct ++ "}%{F" ++ colorBack ++ "} " ++ ws ++ " %{F-}%{B-}"
    , ppVisible = \ws -> wrapClick ws $
        "%{B" ++ colorVis ++ "}%{F" ++ colorBack ++ "} " ++ ws ++ " %{F-}%{B-}"
    , ppHidden  = \ws -> wrapClick ws $
        "%{B" ++ colorOcc ++ "}%{F" ++ colorVis ++ "} " ++ ws ++ " %{F-}%{B-}"
    , ppHiddenNoWindows = \ws -> wrapClick ws $
        "%{F" ++ colorEmp ++ "} " ++ ws ++ " %{F-}"
    , ppSep   = " "
    , ppWsSep = " "
    , ppExtras = [ logScreen, logLayouts, logWinTitles ]
    -- Lambda function missing backslash fixed below
    , ppOrder  = \(ws : _ : _ : ex) -> [ws] ++ ex
    }
  where
    objectPath    = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName    = D.memberName_ "Update"

getWellKnownName dbus = do
    let name = D.busName_ "org.xmonad.Log"
    _ <- D.requestName dbus name
        [ D.nameAllowReplacement
        , D.nameReplaceExisting
        , D.nameDoNotQueue
        ]
    return ()

-- ==========================================================================
-- MAIN
-- ==========================================================================

main = do
    dbus <- D.connectSession
    getWellKnownName dbus

    xmonad $ ewmhFullscreen $ ewmh $ docks $ def
        { terminal           = "wezterm"
        , modMask            = mod4Mask
        , workspaces         = myWorkspaces
        , layoutHook         = myLayout
        , manageHook =
            composeAll
                [ className =? "kmag"  --> doFloat
                , className =? "KMag"  --> doFloat
                , title     =? "KMag"  --> doFloat
                ]
            <+> manageDocks
            <+> manageHook def
        , startupHook        = spawnOnce "sh /home/zeke/.xmonad/autostart.sh"
        , logHook            = dynamicLogWithPP (dbusPP dbus)
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        }
        `additionalKeysP`
        [ ("M-r",        spawn "rofi -show combi -combi-modes 'drun,run,window'")
        , ("M-t",        spawn "rofi -show window")
        , ("<Print>",    spawn "flameshot gui")
        , ("M-l",        spawn "i3lock-fancy")
        , ("M-<Return>", spawn "wezterm")
        , ("M-q",        spawn "xmonad --recompile; xmonad --restart")
        , ("M-v",        spawn "copyq toggle")
        , ("M-w",        kill)
        , ("M-C-q",      io (exitWith ExitSuccess))
        , ("M-<Tab>",    spawn "rofi -show window -show-icons")
        , ("M-C-<Tab>",  sendMessage NextLayout)
        , ("M-S-b",      spawn "polybar-msg cmd toggle")
        , ("M-b",        spawn "~/.config/polybar/toggle-bar.sh main")
        , ("M-C-b",      spawn "~/.config/polybar/toggle-bar.sh second")
        , ("M-f",        sendMessage (JumpToLayout "Full"))
        , ("M-C-f",      sendMessage ToggleStruts >> sendMessage (JumpToLayout "Full"))
        , ("M-C-m",      spawn "sh -c 'pgrep -x kmag >/dev/null && pkill -x kmag || kmag'")
        , ("M-C-S-m",    spawn "pkill -x kmag")
        ]