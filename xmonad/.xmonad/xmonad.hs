-- ~/.xmonad/xmonad.hs
-- XMonad + Polybar, one bar per monitor (Catppuccin Mocha)

{-# LANGUAGE MultiWayIf #-}

import XMonad
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Loggers (logLayoutOnScreen)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare (filterOutWs)

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen, addEwmhWorkspaceSort)
import XMonad.Hooks.Rescreen
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.ManageHook (doFloat, composeAll, (-->))

import qualified XMonad.StackSet as W

import Control.Monad (filterM, forM_, when)

import Data.Char (toLower)
import Data.List (elemIndex, find, isInfixOf)
import System.Exit (exitWith, ExitCode(ExitSuccess))

import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenSupport)

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

myTerminal = "alacritty"

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
-- SCRATCHPADS (dropdown terminal)
-- ==========================================================================

-- Alacritty with class "dropterm" (high-contrast theme in dropterm.toml)
-- running the tmux session "drop": hiding
-- or closing it keeps whatever runs inside (e.g. Claude) alive, and it is
-- reattached on the next toggle. It floats full width and leaves the bottom
-- bar visible (35px out of 1080), with the clock and the "dropdown" badge.
-- To change the size: RationalRect x y width height (0.5 = 50%).
scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "drop"
         "alacritty --config-file ~/.config/alacritty/dropterm.toml --class dropterm -e tmux new-session -A -s drop"
         isDrop
         (customFloating $ W.RationalRect 0 0 1 (1045 / 1080))
    ]

isDrop :: Query Bool
isDrop = className =? "dropterm"

-- Is the dropdown on that screen's workspace?
dropOnWorkspace :: W.Workspace i l Window -> X Bool
dropOnWorkspace w = not . null <$> filterM (runQuery isDrop) (W.integrate' (W.stack w))

-- Hide the dropdown if it is open (before opening something on top of it)
hideDrop :: X ()
hideDrop = do
    cur <- gets (W.workspace . W.current . windowset)
    visible <- dropOnWorkspace cur
    when visible $ namedScratchpadAction scratchpads "drop"

-- ==========================================================================
-- LAYOUTS
-- ==========================================================================

myLayout = avoidStruts $
    F1Layout ||| tiled ||| Mirror tiled ||| noBorders Full ||| Grid ||| threeCol
  where
    tiled    = Tall 1 (3/100) (1/2)
    threeCol = ThreeColMid 1 (3/100) (1/2)

layoutIcon :: String -> String
layoutIcon l
    | "Mirror"   `isInfixOf` l = "\xF1888"
    | "ThreeCol" `isInfixOf` l = "\xF056B"
    | "Tall"     `isInfixOf` l = "\xF0BCC"
    | "Full"     `isInfixOf` l = "\xF0C8"
    | "Grid"     `isInfixOf` l = "\xF009"
    | "F1"       `isInfixOf` l = "\xF0574"
    | otherwise                = polyEsc l

-- ==========================================================================
-- POLYBAR: one bar per monitor
-- ==========================================================================

-- xmonad publishes each screen's text in the _XMONAD_LOG_N property
-- (read by ~/.config/polybar/xmonad-log.sh, one bar per screen).
barsLogHook :: X ()
barsLogHook = do
    screens <- gets (map W.screen . W.screens . windowset)
    forM_ screens $ \s@(S n) ->
        dynamicLogString (filterOutWsPP [scratchpadWorkspaceTag] (screenPP s)) >>= xmonadPropLog' ("_XMONAD_LOG_" ++ show n)

-- Kill all bars and start one per connected monitor
launchBars :: X ()
launchBars = spawn "~/.config/polybar/launch-bars.sh"

-- Polybar markup
fgbg :: String -> String -> String -> String
fgbg f b t = "%{F" ++ f ++ "}%{B" ++ b ++ "}" ++ t ++ "%{B-}%{F-}"

fgc :: String -> String -> String
fgc f t = "%{F" ++ f ++ "}" ++ t ++ "%{F-}"

-- Keep a title containing "%{" from being parsed as markup
polyEsc :: String -> String
polyEsc ('%' : '{' : r) = "% {" ++ polyEsc r
polyEsc (c : r)         = c : polyEsc r
polyEsc []              = []

wrapClick :: String -> String -> String
wrapClick ws content = case elemIndex ws myWorkspaces of
    Just i  -> "%{A1:xdotool set_desktop " ++ show i ++ ":}" ++ content ++ "%{A}"
    Nothing -> content

screenPP :: ScreenId -> PP
screenPP s = def
    { ppCurrent         = \ws -> wrapClick ws $ fgbg colorBack colorAct (" " ++ ws ++ " ")
    , ppVisible         = \ws -> wrapClick ws $ fgbg colorBack colorVis (" " ++ ws ++ " ")
    , ppHidden          = \ws -> wrapClick ws $ fgbg colorVis  colorOcc (" " ++ ws ++ " ")
    , ppHiddenNoWindows = \ws -> wrapClick ws $ fgc  colorEmp (" " ++ ws ++ " ")
    , ppUrgent          = \ws -> wrapClick ws $ fgbg colorBack "#f38ba8" (" " ++ ws ++ " ")
    , ppSep             = " "
    , ppWsSep           = " "
    , ppExtras          = [ logScreenIndicator s
                          , fmap (fmap layoutBlock) (logLayoutOnScreen s)
                          , fmap (fmap (sep ++)) (logWinTitleOnScreen s)
                          , logDropBadge s
                          ]
    , ppOrder           = \xs -> case xs of
                              (ws : _ : _ : ind : lay : win : badge : _) -> [ind, badge, ws, lay, win]
                              _                                  -> xs
    }
  where
    sep           = fgc colorEmp "|" ++ " "
    layoutBlock l = sep ++ layoutIcon l

-- Focused monitor: pink block; unfocused: grey. Hidden with a single
-- monitor (Just "" keeps the ppOrder pattern intact).
logScreenIndicator :: ScreenId -> X (Maybe String)
logScreenIndicator s = do
    ws <- gets windowset
    let single = null (W.visible ws)
        cur    = W.screen (W.current ws)
    pure $ Just $ if
        | single    -> ""
        | cur == s  -> fgbg colorBack colorAct  "  \xF0379   "
        | otherwise -> fgbg colorEmp  colorBack "  \xF0D90   "

-- Orange badge while the dropdown covers that screen. It returns Just ""
-- rather than Nothing when hidden: dynamicLogString drops Nothing extras,
-- which would shift the list and break the ppOrder pattern.
logDropBadge :: ScreenId -> X (Maybe String)
logDropBadge s = do
    ws <- gets windowset
    case find ((== s) . W.screen) (W.current ws : W.visible ws) of
        Nothing -> pure (Just "")
        Just sc -> do
            open <- dropOnWorkspace (W.workspace sc)
            pure $ Just $ if open then fgbg colorBack "#fab387" " \xF018D dropdown " else ""

-- Focused window on that screen, with an icon per application
logWinTitleOnScreen :: ScreenId -> X (Maybe String)
logWinTitleOnScreen s = do
    ws <- gets windowset
    case find ((== s) . W.screen) (W.current ws : W.visible ws) of
        Nothing -> pure Nothing
        Just sc -> case W.focus <$> W.stack (W.workspace sc) of
            Nothing -> pure $ Just $ fgc colorEmp "\xF05B2" ++ " Desktop"
            Just w  -> do
                cls  <- runQuery className w
                name <- show <$> getName w
                let text | null name || cls == name = cls
                         | otherwise                = cls ++ " - " ++ name
                pure $ Just $ appIcon (map toLower cls) ++ " " ++ polyEsc (shorten 45 (filter (/= '\n') text))

appIcon :: String -> String
appIcon c
    | "code"   `isInfixOf` c = fgc "#89b4fa" "\xE70C"
    | "kitty"  `isInfixOf` c = fgc "#f5e0dc" "\xF489"
    | "thunar" `isInfixOf` c = fgc "#f9e2af" "\xF0DCF"
    | "brave"  `isInfixOf` c = fgc "#fab387" "\xE743"
    | otherwise              = fgc colorEmp  "\xF05B2"

-- On monitor (dis)connect: autorandr arranges the screens, then the bars
-- are relaunched, one per monitor.
myRescreen :: RescreenConfig
myRescreen = def
    { randrChangeHook   = spawn "autorandr --change --default horizontal"
    , afterRescreenHook = launchBars
    }

-- ==========================================================================
-- MAIN
-- ==========================================================================

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . addEwmhWorkspaceSort (pure (filterOutWs [scratchpadWorkspaceTag]))
     . ewmh . docks
     . fullscreenSupport
     . rescreenHook myRescreen
     $ def
        { terminal           = myTerminal
        , modMask            = mod4Mask
        , workspaces         = myWorkspaces
        , layoutHook         = myLayout
        , manageHook =
            namedScratchpadManageHook scratchpads
            <+> composeAll
                [ className =? "kmag"  --> doFloat
                , className =? "scrcpy" --> doFloat    -- phone screen (scrcpy)
                , className =? "KMag"  --> doFloat
                , title     =? "KMag"  --> doFloat
                ]
            <+> manageDocks
            <+> manageHook def
        , startupHook        = spawnOnce "sh /home/zeke/.xmonad/autostart.sh" >> launchBars
        , logHook            = barsLogHook >> refocusLastLogHook >> nsHideOnFocusLoss scratchpads
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        }
        `additionalKeysP`
        [ ("M-r",        hideDrop >> spawn "rofi -show combi -combi-modes 'drun,run,window'")
        , ("M-t",        hideDrop >> spawn "rofi -show window")
        , ("M-e",        hideDrop >> spawn "thunar")
        , ("<Print>",    spawn "maim -s -u 2>/dev/null | xclip -selection clipboard -t image/png")
        , ("M-l",        spawn "~/.local/bin/lock-screen")
        , ("M-<Return>", hideDrop >> spawn myTerminal)
        , ("M-q",        spawn "xmonad --recompile; xmonad --restart")
        , ("M-v",        spawn "copyq toggle")
        , ("M-w",        kill)
        , ("M-C-q",      io (exitWith ExitSuccess))
        , ("M-<Tab>",    hideDrop >> spawn "rofi -show window -show-icons")
        , ("M-C-<Tab>",  sendMessage NextLayout)
        , ("M-S-b",      spawn "polybar-msg cmd toggle")
        , ("M-b",        spawn "~/.config/polybar/toggle-bar.sh main")
        , ("M-C-b",      spawn "~/.config/polybar/toggle-bar.sh second")
        , ("M-f",        sendMessage (JumpToLayout "Full"))
        , ("M-C-f",      sendMessage ToggleStruts >> sendMessage (JumpToLayout "Full"))
        , ("M-C-m",      spawn "sh -c 'pgrep -x kmag >/dev/null && pkill -x kmag || kmag'")
        , ("M-C-S-m",    spawn "pkill -x kmag")
        -- Dropdown terminal (toggle): Ctrl + Win + T
        , ("M-C-t",      namedScratchpadAction scratchpads "drop")
        -- Volume and brightness keys show an OSD (dunst progress bar)
        , ("<XF86AudioRaiseVolume>",  spawn "~/.local/bin/osd-volume up")
        , ("<XF86AudioLowerVolume>",  spawn "~/.local/bin/osd-volume down")
        , ("<XF86AudioMute>",         spawn "~/.local/bin/osd-volume mute")
        , ("<XF86MonBrightnessUp>",   spawn "~/.local/bin/osd-brightness up")
        , ("<XF86MonBrightnessDown>", spawn "~/.local/bin/osd-brightness down")
        ]
