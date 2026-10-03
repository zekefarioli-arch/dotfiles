-- ~/.xmonad/xmonad.hs
-- XMonad + Polybar, una barra por monitor (Catppuccin Mocha)

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
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.ManageHook (doFloat, composeAll, (-->))

import qualified XMonad.StackSet as W

import Control.Monad (forM_)

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

-- wezterm no está en los repos de Fedora: si falta, usa alacritty
myTerminal = "sh -c 'command -v wezterm >/dev/null && exec wezterm || exec alacritty'"

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
-- SCRATCHPADS (terminal desplegable)
-- ==========================================================================

-- wezterm (clase "dropterm", tema de alto contraste en dropterm.lua) con la
-- sesión de tmux "drop":
-- al ocultarlo o cerrarlo, lo que corre adentro (p. ej. Claude) sigue vivo
-- y se reconecta al volver a abrirlo. Flota a pantalla completa.
-- Para cambiar el tamaño: RationalRect x y ancho alto (0.5 = 50%).
scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "drop"
         "wezterm --config-file ~/.config/wezterm/dropterm.lua start --class dropterm -- tmux new-session -A -s drop"
         (className =? "dropterm")
         (customFloating $ W.RationalRect 0 0 1 1)
    ]

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
-- POLYBAR: una barra por monitor
-- ==========================================================================

-- xmonad publica el texto de cada pantalla en la propiedad _XMONAD_LOG_N
-- (la lee ~/.config/polybar/xmonad-log.sh, una barra por pantalla).
barsLogHook :: X ()
barsLogHook = do
    screens <- gets (map W.screen . W.screens . windowset)
    forM_ screens $ \s@(S n) ->
        dynamicLogString (filterOutWsPP [scratchpadWorkspaceTag] (screenPP s)) >>= xmonadPropLog' ("_XMONAD_LOG_" ++ show n)

-- Mata todas las barras y lanza una por monitor conectado
launchBars :: X ()
launchBars = spawn "~/.config/polybar/launch-bars.sh"

-- Marcado de polybar
fgbg :: String -> String -> String -> String
fgbg f b t = "%{F" ++ f ++ "}%{B" ++ b ++ "}" ++ t ++ "%{B-}%{F-}"

fgc :: String -> String -> String
fgc f t = "%{F" ++ f ++ "}" ++ t ++ "%{F-}"

-- Evita que un título con "%{" se interprete como marcado
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
                          ]
    , ppOrder           = \xs -> case xs of
                              (ws : _ : _ : ind : lay : win : _) -> [ind, ws, lay, win]
                              _                                  -> xs
    }
  where
    sep           = fgc colorEmp "|" ++ " "
    layoutBlock l = sep ++ layoutIcon l

-- Monitor con foco: bloque rosa; sin foco: gris
logScreenIndicator :: ScreenId -> X (Maybe String)
logScreenIndicator s = do
    cur <- gets (W.screen . W.current . windowset)
    pure $ Just $ if cur == s
        then fgbg colorBack colorAct  "  \xF0379   "
        else fgbg colorEmp  colorBack "  \xF0D90   "

-- Ventana con foco en esa pantalla, con ícono según la aplicación
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

-- Al conectar/desconectar monitores: autorandr acomoda las pantallas y
-- después se relanzan las barras, una por monitor.
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
                , className =? "KMag"  --> doFloat
                , title     =? "KMag"  --> doFloat
                ]
            <+> manageDocks
            <+> manageHook def
        , startupHook        = spawnOnce "sh /home/zeke/.xmonad/autostart.sh" >> launchBars
        , logHook            = barsLogHook
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        }
        `additionalKeysP`
        [ ("M-r",        spawn "rofi -show combi -combi-modes 'drun,run,window'")
        , ("M-t",        spawn "rofi -show window")
        , ("M-e",        spawn "thunar")
        , ("<Print>",    spawn "maim -s -u 2>/dev/null | xclip -selection clipboard -t image/png")
        , ("M-l",        spawn "~/.local/bin/lock-screen")
        , ("M-<Return>", spawn myTerminal)
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
        -- Terminal desplegable (toggle): Ctrl + Win + T
        , ("M-C-t",      namedScratchpadAction scratchpads "drop")
        , ("<XF86AudioRaiseVolume>",  spawn "wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+")
        , ("<XF86AudioLowerVolume>",  spawn "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
        , ("<XF86AudioMute>",         spawn "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
        , ("<XF86MonBrightnessUp>",   spawn "brightnessctl set +10%")
        , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
        ]
