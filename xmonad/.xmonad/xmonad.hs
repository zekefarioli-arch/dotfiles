-- ~/.xmonad/xmonad.hs
-- XMonad + Polybar DBus (Catppuccin Mocha)
-- All comments in English

import XMonad
import XMonad.Config (def)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)

import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.StackSet as W

import System.Exit (exitWith, ExitCode(ExitSuccess))

import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders  -- Removes borders in fullscreen (Full)

-- Needed for per-screen focused window title (WIN0 / WIN1)
import XMonad.Util.NamedWindows (getName)
import Data.Char (toUpper) -- Para formatear nombres si se desea

-- ==========================================================================
-- COLORS (Catppuccin Mocha)
-- ==========================================================================
colorBack = "#1e1e2e"
colorAct  = "#f5c2e7" -- Pink (Focused)
colorVis  = "#89b4fa" -- Blue (Visible not focused)
colorOcc  = "#313244" -- Dark Grey (Occupied)
colorEmp  = "#6c7086" -- Light Grey (Empty)

-- Window Borders
myBorderWidth = 2
myNormColor   = "#313244" -- Unfocused windows
myFocusColor  = "#f5c2e7" -- Focused window

-- ==========================================================================
-- WORKSPACES
-- ==========================================================================
myWorkspaces = ["1","2","3","4","5","6","7","8"]

-- ==========================================================================
-- LAYOUTS
-- ==========================================================================
myLayout = avoidStruts $ tiled ||| Mirror tiled ||| noBorders Full ||| Grid ||| threeCol
  where
    tiled    = Tall nmaster delta ratio
    threeCol = ThreeColMid nmaster delta ratio
    nmaster  = 1
    ratio    = 1/2
    delta    = 3/100

-- ==========================================================================
-- CUSTOM LOGGERS (For Polybar via DBus)
-- ==========================================================================

-- 1) Active screen ID (0, 1, ...)
logScreen :: X (Maybe String)
logScreen = do
  s <- gets windowset
  return $ Just $ "SCREEN:" ++ show (fromIntegral (W.screen (W.current s)) :: Int)

-- 2) Layouts for ALL screens
logLayouts :: X (Maybe String)
logLayouts = do
  ws <- gets windowset
  let allScreens = W.current ws : W.visible ws
      formatScreen s =
        "LAY" ++ show (fromIntegral (W.screen s) :: Int)
        ++ ":" ++ description (W.layout (W.workspace s))
  return $ Just $ unwords $ map formatScreen allScreens

-- 3) Focused window title PER screen (CORREGIDO: Programa - Título)
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
          cls <- runQuery className w -- Obtiene el nombre de la app (ej: kitty, code)
          name <- fmap show (getName w) -- Obtiene el título de la ventana
          -- Si la clase y el nombre son iguales, solo muestra uno. Si no, Programa - Título
          return $ if cls == name || null name 
                   then cls 
                   else cls ++ " - " ++ name
      let clean = map (\c -> if c == '\n' then ' ' else c) titleString
      return $ "WIN" ++ sid ++ ":" ++ clean

-- Make workspaces clickable
wrapClick ws content = "%{A1:xdotool key super+" ++ ws ++ ":}" ++ content ++ "%{A}"

-- DBus PP: what we send to Polybar
dbusPP dbus = def
  { ppOutput = \str -> do
      let signal = (D.signal objectPath interfaceName memberName)
                    { D.signalBody = [D.toVariant str] }
      D.emit dbus signal
  , ppCurrent = \ws -> wrapClick ws $ "%{B" ++ colorAct ++ "}%{F" ++ colorBack ++ "} " ++ ws ++ " %{F-}%{B-}"
  , ppVisible = \ws -> wrapClick ws $ "%{B" ++ colorVis ++ "}%{F" ++ colorBack ++ "} " ++ ws ++ " %{F-}%{B-}"
  , ppHidden  = \ws -> wrapClick ws $ "%{B" ++ colorOcc ++ "}%{F" ++ colorVis ++ "} " ++ ws ++ " %{F-}%{B-}"
  , ppHiddenNoWindows = \ws -> wrapClick ws $ "%{F" ++ colorEmp ++ "} " ++ ws ++ " %{F-}"
  , ppSep   = " "
  , ppWsSep = " "
  , ppExtras = [ logScreen, logLayouts, logWinTitles ]
  , ppOrder  = \(ws : _ : _ : ex) -> [ws] ++ ex
  }
  where
    objectPath    = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName    = D.memberName_ "Update"

getWellKnownName dbus = do
  let name = D.busName_ "org.xmonad.Log"
  _ <- D.requestName dbus name [ D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue ]
  return ()

-- ==========================================================================
-- MAIN
-- ==========================================================================
main = do
  dbus <- D.connectSession
  getWellKnownName dbus

  xmonad $ ewmhFullscreen $ ewmh $ docks $ def
    { terminal           = "kitty"
    , modMask            = mod4Mask
    , workspaces         = myWorkspaces
    , layoutHook         = myLayout
    , manageHook         = manageDocks <+> manageHook def
    , startupHook        = spawnOnce "sh /home/zeke/.xmonad/autostart.sh"
    , logHook            = dynamicLogWithPP (dbusPP dbus)
     
    -- Border configuration
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    }
    `additionalKeysP`
    [ ("M-r",          spawn "rofi -show drun")
    , ("<Print>",      spawn "flameshot gui")
    , ("M-l",          spawn "i3lock-fancy")
    , ("M-<Return>",   spawn "kitty")
    , ("M-q",          spawn "xmonad --recompile; xmonad --restart")
    , ("M-v",          spawn "copyq toggle")
    , ("M-w",          kill)
    , ("M-C-q",        io (exitWith ExitSuccess))
    , ("M-<Tab>",      sendMessage NextLayout)
    , ("M-S-b",          spawn "polybar-msg cmd toggle")
    -- Toggle Barra Principal (Monitor 0 / VGA)
    , ("M-b",          spawn "~/.config/polybar/toggle-bar.sh main")
    -- Toggle Barra Secundaria (Monitor 1 / DVI)
    , ("M-C-b",        spawn "~/.config/polybar/toggle-bar.sh second")
    ]