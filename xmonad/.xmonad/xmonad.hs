-- ~/.xmonad/xmonad.hs
-- XMonad + Polybar, one bar per monitor (Catppuccin Mocha)

{-# LANGUAGE MultiWayIf #-}

import XMonad
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Loggers (logLayoutOnScreen)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.WorkspaceCompare (filterOutWs)

import XMonad.Actions.CycleWS (nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFloatDep)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen, addEwmhWorkspaceSort)
import XMonad.Hooks.Rescreen
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.ManageHook (doFloat, composeAll, (-->))

import qualified XMonad.StackSet as W

import Control.Monad (filterM, forM_, unless, when)

import Data.Char (toLower)
import Data.List (elemIndex, find, intercalate, isInfixOf, isPrefixOf, sortOn, stripPrefix)
import qualified Data.Map as M
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, doesFileExist, getHomeDirectory, getXdgDirectory, renameFile)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.FilePath ((</>))

import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier (LayoutModifier (..), ModifiedLayout (..))
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

-- Float a window in the bottom-right corner of its screen, keeping its own
-- size: 10px from the right edge and 10px above the bar (35px of 1080).
bottomRightCorner :: ManageHook
bottomRightCorner = doFloatDep $ \(W.RationalRect _ _ w h) ->
    W.RationalRect (1 - w - 10 / 1920) (1 - h - (35 + 10) / 1080) w h

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

myLayout = avoidStruts $ ModifiedLayout WebAppCrop $
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
                -- Known applications (the icon says which one) show only the window title,
                -- without Brave's " - Brave" suffix; others keep "class - title". Max 32
                -- characters: the pill-shaped bar leaves little room, and long titles ran
                -- into the modules on the right. Rejected: tying the limit to the screen
                -- width (it also depends on the Wi-Fi name and the battery).
                let c     = map toLower cls
                    title = maybe name reverse (stripPrefix (reverse " - Brave") (reverse name))
                    text | null name || cls == name = cls
                         | knownApp c               = title
                         | otherwise                = cls ++ " - " ++ title
                pure $ Just $ appIcon c ++ " " ++ polyEsc (shorten 32 (filter (/= '\n') text))

-- Web apps made by `webapp --panel` have the class WebPanel-<id>
isWebPanel :: String -> Bool
isWebPanel = ("WebPanel-" `isPrefixOf`)

knownApp :: String -> Bool
knownApp c = isWebPanel' || any (`isInfixOf` c) ["code", "kitty", "alacritty", "dropterm", "thunar", "brave"]
  where isWebPanel' = "webpanel-" `isPrefixOf` c

appIcon :: String -> String
appIcon c
    | "code"   `isInfixOf` c = fgc "#89b4fa" "\xE70C"
    | "kitty"  `isInfixOf` c = fgc "#f5e0dc" "\xF489"
    | "alacritty" `isInfixOf` c || "dropterm" `isInfixOf` c = fgc "#f5e0dc" "\xF489"
    | "webpanel-" `isPrefixOf` c = fgc "#89b4fa" "\xF059F"
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

-- ==========================================================================
-- ACTIONS AND KEYBINDINGS
-- ==========================================================================

-- Every action xmonad can run, with or without a shortcut. The shortcut
-- editor and the cheat sheet read this catalog from ~/.cache/xmonad/actions.tsv
-- (written at startup by exportActions).
data Action = Action
    { actCategory :: String
    , actName     :: String     -- stable id used in the keys file
    , actDesc     :: String
    , actKeys     :: [String]   -- default shortcuts (EZConfig syntax)
    , actRun      :: X ()
    }

builtinActions :: [Action]
builtinActions =
    [ Action "Help" "show-shortcuts" "Show the shortcut list" ["M-<F1>", "M-/", "M-<XF86AudioMute>"] (hideDrop >> spawn "~/.local/bin/keybinds")
    , Action "Help" "edit-shortcuts" "Edit the shortcuts" ["M-S-<F1>", "M-S-/"] (hideDrop >> spawn "~/.local/bin/keys-editor")

    , Action "Apps" "terminal" "Terminal (Alacritty)" ["M-<Return>", "M-S-<Return>"] (hideDrop >> spawn myTerminal)
    , Action "Apps" "dropdown-terminal" "Dropdown terminal with tmux" ["M-C-t"] (namedScratchpadAction scratchpads "drop")
    , Action "Apps" "launcher" "Rofi: apps, commands and windows" ["M-r"] (hideDrop >> spawn "rofi -show combi -combi-modes 'drun,run,window'")
    , Action "Apps" "run-command" "dmenu: run a command" ["M-p"] (spawn "dmenu_run")
    , Action "Apps" "file-manager" "File manager (Thunar)" ["M-e"] (hideDrop >> spawn "thunar")
    , Action "Apps" "clipboard" "Clipboard history (CopyQ)" ["M-v"] (spawn "copyq toggle")
    , Action "Apps" "browser" "Web browser (Brave)" [] (spawn "brave-browser")
    , Action "Apps" "phone-screen" "Phone screen on the laptop (scrcpy)" [] (spawn "scrcpy --window-title Phone")

    , Action "Windows" "close-window" "Close the focused window" ["M-w", "M-S-c"] kill
    , Action "Windows" "window-switcher" "Window switcher with icons (rofi)" ["M-<Tab>"] (hideDrop >> spawn "rofi -show window -show-icons")
    , Action "Windows" "window-list" "Window list (rofi)" ["M-t"] (hideDrop >> spawn "rofi -show window")
    , Action "Windows" "focus-next" "Focus the next window" ["M-j"] (windows W.focusDown)
    , Action "Windows" "focus-previous" "Focus the previous window" ["M-k", "M-S-<Tab>"] (windows W.focusUp)
    , Action "Windows" "focus-master" "Focus the master window" ["M-m"] (windows W.focusMaster)
    , Action "Windows" "swap-next" "Swap with the next window" ["M-S-j"] (windows W.swapDown)
    , Action "Windows" "swap-previous" "Swap with the previous window" ["M-S-k"] (windows W.swapUp)
    , Action "Windows" "swap-master" "Swap with the master window" [] (windows W.swapMaster)
    , Action "Windows" "sink-window" "Put a floating window back into the layout" ["M-S-t"] (withFocused $ windows . W.sink)
    , Action "Windows" "refresh" "Resize windows to the current layout" ["M-n"] refresh

    , Action "Layouts" "next-layout" "Next layout" ["M-<Space>", "M-C-<Tab>"] (sendMessage NextLayout)
    , Action "Layouts" "reset-layout" "Reset to the first layout" ["M-S-<Space>"] (asks (layoutHook . config) >>= setLayout)
    , Action "Layouts" "full-layout" "Full screen layout" ["M-f"] (sendMessage (JumpToLayout "Full"))
    , Action "Layouts" "full-layout-no-bar" "Full screen layout and hide the bar" ["M-C-f"] (sendMessage ToggleStruts >> sendMessage (JumpToLayout "Full"))
    , Action "Layouts" "toggle-gaps" "Let windows cover the bar / leave room for it" [] (sendMessage ToggleStruts)
    , Action "Layouts" "shrink-master" "Shrink the master area" ["M-h"] (sendMessage Shrink)
    , Action "Layouts" "expand-master" "Expand the master area" [] (sendMessage Expand)
    , Action "Layouts" "more-master" "More windows in the master area" ["M-,"] (sendMessage (IncMasterN 1))
    , Action "Layouts" "fewer-master" "Fewer windows in the master area" ["M-."] (sendMessage (IncMasterN (-1)))
    ]
    ++ concat
    [ [ Action "Workspaces" ("view-workspace-" ++ n) ("Go to workspace " ++ n) ["M-" ++ n] (windows (W.greedyView ws))
      , Action "Workspaces" ("move-to-workspace-" ++ n) ("Move the window to workspace " ++ n) ["M-S-" ++ n] (windows (W.shift ws))
      , Action "Workspaces" ("move-and-follow-" ++ n) ("Move the window to workspace " ++ n ++ " and go there") [] (windows (W.greedyView ws . W.shift ws))
      ]
    | (n, ws) <- zip (map show [1 :: Int ..]) myWorkspaces ]
    ++
    [ Action "Monitors" "focus-previous-monitor" "Focus the previous monitor" ["M-S-,"] prevScreen
    , Action "Monitors" "focus-next-monitor" "Focus the next monitor" ["M-S-."] nextScreen
    , Action "Monitors" "move-to-previous-monitor" "Move the window to the previous monitor" ["M-C-,"] (shiftPrevScreen >> prevScreen)
    , Action "Monitors" "move-to-next-monitor" "Move the window to the next monitor" ["M-C-."] (shiftNextScreen >> nextScreen)
    ]
    ++ concat
    [ [ Action "Monitors" ("focus-monitor-" ++ show (i + 1)) ("Focus monitor " ++ show (i + 1)) [] (onScreen i W.view)
      , Action "Monitors" ("move-to-monitor-" ++ show (i + 1)) ("Move the window to monitor " ++ show (i + 1)) ["M-S-" ++ [k]] (onScreen i W.shift)
      ]
    | (i, k) <- zip [0 ..] "wer" ]
    ++
    [ Action "Notifications" "show-last-notification" "Show the last notification again" ["M-S-n"] (spawn "dunstctl history-pop")
    , Action "Notifications" "close-notifications" "Close all notifications" ["M-C-n"] (spawn "dunstctl close-all")

    , Action "Bar" "toggle-main-bar" "Show / hide the main bar" ["M-b"] (spawn "~/.config/polybar/toggle-bar.sh main")
    , Action "Bar" "toggle-secondary-bar" "Show / hide the secondary bar" ["M-C-b"] (spawn "~/.config/polybar/toggle-bar.sh second")
    , Action "Bar" "toggle-all-bars" "Show / hide all bars" ["M-S-b"] (spawn "polybar-msg cmd toggle")

    , Action "System" "lock-screen" "Lock the screen" ["M-l"] (spawn "~/.local/bin/lock-screen")
    , Action "System" "restart-xmonad" "Recompile and restart xmonad (keeps windows)" ["M-q"] (spawn "xmonad --recompile; xmonad --restart")
    , Action "System" "log-out" "Log out" ["M-C-q", "M-S-q"] (io (exitWith ExitSuccess))
    , Action "System" "screenshot" "Screenshot of an area (clipboard + ~/Pictures/Screenshots)" ["<Print>"] (spawn "~/.local/bin/screenshot area")
    , Action "System" "screenshot-window" "Screenshot of the active window" ["M1-<Print>"] (spawn "~/.local/bin/screenshot window")
    , Action "System" "screenshot-monitor" "Screenshot of the monitor under the mouse" ["S-<Print>"] (spawn "~/.local/bin/screenshot monitor")
    , Action "System" "screenshot-full" "Screenshot of every monitor" ["C-<Print>"] (spawn "~/.local/bin/screenshot full")
    , Action "System" "suspend" "Suspend (sleep)" [] (spawn "systemctl suspend")
    , Action "System" "caffeine" "Caffeine on / off (keep the screen awake)" [] (spawn "~/.config/polybar/scripts/caffeine_toggle.sh")
    , Action "System" "magnifier" "Magnifier on / off (KMag)" ["M-C-m"] (spawn "sh -c 'pgrep -x kmag >/dev/null && pkill -x kmag || kmag'")
    , Action "System" "close-magnifier" "Close the magnifier" ["M-C-S-m"] (spawn "pkill -x kmag")

    , Action "Media" "volume-up" "Volume +5% with on-screen indicator" ["<XF86AudioRaiseVolume>"] (spawn "~/.local/bin/osd-volume up")
    , Action "Media" "volume-down" "Volume -5% with on-screen indicator" ["<XF86AudioLowerVolume>"] (spawn "~/.local/bin/osd-volume down")
    , Action "Media" "volume-mute" "Mute / unmute" ["<XF86AudioMute>"] (spawn "~/.local/bin/osd-volume mute")
    , Action "Media" "mic-mute" "Mute / unmute the microphone" [] (spawn "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle")
    , Action "Media" "brightness-up" "Brightness +5% with on-screen indicator" ["<XF86MonBrightnessUp>"] (spawn "~/.local/bin/osd-brightness up")
    , Action "Media" "brightness-down" "Brightness -5% with on-screen indicator" ["<XF86MonBrightnessDown>"] (spawn "~/.local/bin/osd-brightness down")
    ]
  where
    onScreen i f = screenWorkspace (S i) >>= flip whenJust (windows . f)

-- Custom actions added by the user (shortcut editor or by hand), read from
-- ~/.xmonad/actions.conf at startup: "category<TAB>name<TAB>description<TAB>command"
-- per line. They run a shell command and get shortcuts like any other action.
customActionsFile :: IO FilePath
customActionsFile = (</> ".xmonad/actions.conf") <$> getHomeDirectory

data CustomAction = CustomAction { caAction :: Action, caCommand :: String }

loadCustomActions :: IO [CustomAction]
loadCustomActions = do
    file <- customActionsFile
    exists <- doesFileExist file
    if not exists then pure [] else do
        txt <- readFile file
        let parse l = case splitTabs l of
                [cat, name, desc, cmd] | not (null name) && take 1 cat /= "#" ->
                    [CustomAction (Action cat name desc [] (spawn cmd)) cmd]
                _ -> []
        length txt `seq` pure (concatMap parse (lines txt))
  where
    splitTabs l = case break (== '\t') l of
        (a, []) -> [a]
        (a, _ : rest) -> a : splitTabs rest

-- Built-in actions first; a custom action cannot replace a built-in one
allActions :: [CustomAction] -> [Action]
allActions cs = builtinActions ++ [ caAction c | c <- cs, actName (caAction c) `notElem` map actName builtinActions ]

-- Shortcuts chosen by the user, read from ~/.xmonad/keys.conf at startup:
-- "action-name shortcut shortcut ..." per line. An action missing from the
-- file keeps its default shortcuts; an action listed alone has none.
-- Changes apply with "xmonad --restart" (no recompile needed).
type KeyOverrides = M.Map String [String]

loadKeys :: IO KeyOverrides
loadKeys = do
    file <- (</> ".xmonad/keys.conf") <$> getHomeDirectory
    exists <- doesFileExist file
    if not exists then pure M.empty else do
        txt <- readFile file
        let entries = [ (n, ks) | l <- lines txt, (n : ks) <- [words l], take 1 n /= "#" ]
        length txt `seq` pure (M.fromList entries)

actionKeys :: KeyOverrides -> Action -> [String]
actionKeys o a = M.findWithDefault (actKeys a) (actName a) o

-- Keybindings built from the catalog (they replace xmonad's default keys)
myKeys :: [Action] -> KeyOverrides -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys acts o c = mkKeymap c [ (k, actRun a) | a <- acts, k <- actionKeys o a ]

-- Warn (without breaking anything) about unknown actions, invalid shortcuts
-- and shortcuts assigned to more than one action in keys.conf.
checkKeys :: [CustomAction] -> KeyOverrides -> X ()
checkKeys cs o = do
    c <- asks config
    let parse k = M.keys (mkKeymap c [(k, pure ())])
        acts    = allActions cs
        unknown = [ n | n <- M.keys o, n `notElem` map actName acts ]
        clashes = [ actName (caAction ca) | ca <- cs, actName (caAction ca) `elem` map actName builtinActions ]
        invalid = [ k | a <- acts, k <- actionKeys o a, null (parse k) ]
        owners  = M.fromListWith (++) [ (kc, [(k, actName a)]) | a <- acts, k <- actionKeys o a, kc <- parse k ]
        dups    = [ fst (head us) ++ " is used by " ++ intercalate ", " (map snd us)
                  | us <- M.elems owners, length us > 1 ]
        problems = map ("unknown action: " ++) unknown
                ++ map ("custom action has a built-in name: " ++) clashes
                ++ map ("invalid shortcut: " ++) invalid ++ dups
    unless (null problems) $
        safeSpawn "notify-send" ["-u", "critical", "xmonad: problems in keys.conf", unlines problems]

-- Write the catalog for the shortcut editor and the cheat sheet, tab separated:
-- category, name, description, current shortcuts, default shortcuts
-- (shortcuts separated by spaces), "custom" for custom actions and their command.
exportActions :: [CustomAction] -> KeyOverrides -> X ()
exportActions cs o = io $ do
    dir <- getXdgDirectory XdgCache "xmonad"
    createDirectoryIfMissing True dir
    -- Written to a temporary file and renamed, so readers never see it half written
    let file = dir </> "actions.tsv"
    writeFile (file ++ ".tmp") $ unlines
        [ intercalate "\t" ([actCategory a, actName a, actDesc a, unwords (actionKeys o a), unwords (actKeys a)]
                             ++ maybe [] (\c -> ["custom", c]) (lookup (actName a) commands))
        | a <- allActions cs ]
    renameFile (file ++ ".tmp") file
  where
    commands = [ (actName (caAction c), caCommand c) | c <- cs ]

-- WEB APPS WITH THE CLAUDE PANEL
-- `webapp --panel` windows are normal Brave windows (the Claude side panel does
-- not exist in --app windows), so Brave shows its tab strip and toolbar. Brave
-- cannot hide them, but the layout can push them out of sight: the rectangle of
-- every WebPanel-* window is extended upwards by webappChromePx, so only the
-- page (and the side panel) is seen. At the top edge of the screen the toolbar
-- falls off the screen; anywhere else it falls under the window above, because
-- these windows are put at the bottom of the stacking order (xmonad stacks the
-- layout's list from top to bottom, so they go last, upper ones first).
-- The window stays tiled with the other windows of the workspace.
-- Rejected: fullscreen state (Brave hides its UI, but ewmhFullscreen covers the
-- bar and the other windows; a hook that sank the window again was a hack
-- around two EWMH handlers) and floating windows (they leave the layout).
-- A first version only cropped windows at the top edge: a web app in the second
-- row of the layout showed its toolbar again.
-- Tune webappChromePx to the height of Brave's tab strip + toolbar (with the
-- bookmarks bar hidden).
webappChromePx :: Dimension
webappChromePx = 86

data WebAppCrop a = WebAppCrop deriving (Show, Read)

instance LayoutModifier WebAppCrop Window where
  redoLayout WebAppCrop _ _ wrs = do
    tagged <- mapM tag wrs
    let others = [wr | (False, wr) <- tagged]
        panels = sortOn (rect_y . snd) [wr | (True, wr) <- tagged]
    return (others ++ map crop panels, Nothing)
    where
      tag wr@(w, _) = do
        isPanel <- runQuery (fmap isWebPanel className) w
        return (isPanel, wr)
      crop (w, r) = ( w
                    , r { rect_y = rect_y r - fromIntegral webappChromePx
                        , rect_height = rect_height r + webappChromePx } )

main :: IO ()
main = do
  keyOverrides <- loadKeys
  customActions <- loadCustomActions
  xmonad
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
                , className =? "calendar-popup" --> bottomRightCorner  -- calendar from the bar's date
                , className =? "KMag"  --> doFloat
                , title     =? "KMag"  --> doFloat
                ]
            <+> manageDocks
            <+> manageHook def
        , startupHook        = spawnOnce "sh /home/zeke/.xmonad/autostart.sh" >> launchBars >> exportActions customActions keyOverrides >> checkKeys customActions keyOverrides
        , logHook            = barsLogHook >> refocusLastLogHook >> nsHideOnFocusLoss scratchpads
        , keys               = myKeys (allActions customActions) keyOverrides
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        }
