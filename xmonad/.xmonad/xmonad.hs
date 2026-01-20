import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Config (def)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Hooks.ManageDocks
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Hooks.DynamicLog

import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.StackSet as W

import System.Exit (exitWith, ExitCode(ExitSuccess))

import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns

-- ==========================================================================
-- COLORES (Catppuccin Mocha)
-- ==========================================================================
colorBack = "#1e1e2e"
colorAct  = "#f5c2e7" -- Rosa (Enfocado)
colorVis  = "#89b4fa" -- Azul (Visible no enfocado)
colorOcc  = "#313244" -- Gris Oscuro (Oculto con ventanas)
colorEmp  = "#6c7086" -- Gris Claro (Vacío)

myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6","7","8"]

-- ==========================================================================
-- LAYOUTS
-- ==========================================================================
myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full ||| Grid ||| threeCol
  where
    tiled    = Tall nmaster delta ratio
    threeCol = ThreeColMid nmaster delta ratio
    nmaster  = 1
    ratio    = 1/2
    delta    = 3/100

-- ==========================================================================
-- LOGGERS PERSONALIZADOS (La magia para Polybar)
-- ==========================================================================

-- 1. Detectar ID de pantalla activa (0, 1, etc.) CORREGIDO
logScreen :: X (Maybe String)
logScreen = do
  s <- gets windowset
  -- Usamos fromIntegral para que devuelva "0" y no "S 0"
  return $ Just $ "SCREEN:" ++ show (fromIntegral (W.screen (W.current s)) :: Int)

-- 2. Detectar Layouts de TODAS las pantallas (LAY0:Tall LAY1:Grid...) CORREGIDO
logLayouts :: X (Maybe String)
logLayouts = do
  ws <- gets windowset
  let allScreens = W.current ws : W.visible ws
  -- Usamos fromIntegral para que el formato sea "LAY0:..." y el script lo entienda
  let formatScreen s = "LAY" ++ show (fromIntegral (W.screen s) :: Int) ++ ":" ++ description (W.layout (W.workspace s))
  return $ Just $ unwords $ map formatScreen allScreens

-- Función para hacer click en los workspaces
wrapClick :: String -> String -> String
wrapClick ws content = "%{A1:xdotool key super+" ++ ws ++ ":}" ++ content ++ "%{A}"

-- Configuración de lo que se envía a Polybar por DBus
dbusPP :: D.Client -> PP
dbusPP dbus = def
  { ppOutput = \str -> do
      let signal = (D.signal objectPath interfaceName memberName) { D.signalBody = [D.toVariant str] }
      D.emit dbus signal
   
  -- Colores de los estados de workspaces
  , ppCurrent = \ws -> wrapClick ws $ "%{B" ++ colorAct ++ "}%{F" ++ colorBack ++ "} " ++ ws ++ " %{F-}%{B-}"
  , ppVisible = \ws -> wrapClick ws $ "%{B" ++ colorVis ++ "}%{F" ++ colorBack ++ "} " ++ ws ++ " %{F-}%{B-}"
  , ppHidden  = \ws -> wrapClick ws $ "%{B" ++ colorOcc ++ "}%{F" ++ colorVis ++ "} " ++ ws ++ " %{F-}%{B-}"
  , ppHiddenNoWindows = \ws -> wrapClick ws $ "%{F" ++ colorEmp ++ "} " ++ ws ++ " %{F-}"
   
  , ppSep     = "" 
  , ppWsSep   = ""
   
  -- AGREGAMOS LOS EXTRAS: ID de pantalla y Lista de Layouts
  , ppExtras  = [ logScreen, logLayouts ]
   
  -- ORDEN: Solo mandamos los Workspaces y los Extras (el script procesa el resto)
  -- 'ex' contiene [SCREEN:X, LAY0:Tall LAY1:Grid]
  , ppOrder   = \(ws:_:_:ex) -> [ws] ++ ex
  }
  where
    objectPath    = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName    = D.memberName_ "Update"

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  let name = D.busName_ "org.xmonad.Log"
  _ <- D.requestName dbus name [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

-- ==========================================================================
-- MAIN
-- ==========================================================================
main :: IO ()
main = do
  dbus <- D.connectSession
  getWellKnownName dbus

  xmonad $ ewmhFullscreen $ ewmh $ docks $ def
    { terminal    = "kitty"
    , modMask     = mod4Mask
    , workspaces  = myWorkspaces
    , layoutHook  = myLayout
    , manageHook  = manageDocks <+> manageHook def
    , startupHook = spawnOnce "sh /home/zeke/.xmonad/autostart.sh"
    , logHook     = dynamicLogWithPP (dbusPP dbus)
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
    ]