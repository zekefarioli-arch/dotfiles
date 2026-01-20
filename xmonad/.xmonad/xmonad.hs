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
import XMonad.Layout.NoBorders  -- Necessary for removing borders in Fullscreen

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
myNormColor   = "#313244" -- Dark grey for unfocused windows
myFocusColor  = "#f5c2e7" -- Pink for focused windows

myWorkspaces :: [String]
myWorkspaces = ["1","2","3","4","5","6","7","8"]

-- ==========================================================================
-- LAYOUTS
-- ==========================================================================
-- 'noBorders Full' ensures no pink border when in fullscreen mode
myLayout = avoidStruts $ tiled ||| Mirror tiled ||| noBorders Full ||| Grid ||| threeCol
  where
    tiled    = Tall nmaster delta ratio
    threeCol = ThreeColMid nmaster delta ratio
    nmaster  = 1
    ratio    = 1/2
    delta    = 3/100

-- ==========================================================================
-- CUSTOM LOGGERS (The magic for Polybar)
-- ==========================================================================

-- 1. Detect Active Screen ID (0, 1) fixed to Int
logScreen :: X (Maybe String)
logScreen = do
  s <- gets windowset
  -- Use fromIntegral to send "0" instead of "S 0"
  return $ Just $ "SCREEN:" ++ show (fromIntegral (W.screen (W.current s)) :: Int)

-- 2. Detect Layouts of ALL screens
logLayouts :: X (Maybe String)
logLayouts = do
  ws <- gets windowset
  let allScreens = W.current ws : W.visible ws
  -- Format: LAY0:Tall LAY1:Grid (Using fromIntegral for the ID)
  let formatScreen s = "LAY" ++ show (fromIntegral (W.screen s) :: Int) ++ ":" ++ description (W.layout (W.workspace s))
  return $ Just $ unwords $ map formatScreen allScreens

-- Function to make workspaces clickable
wrapClick :: String -> String -> String
wrapClick ws content = "%{A1:xdotool key super+" ++ ws ++ ":}" ++ content ++ "%{A}"

-- Configuration of what is sent to Polybar via DBus
dbusPP :: D.Client -> PP
dbusPP dbus = def
  { ppOutput = \str -> do
      let signal = (D.signal objectPath interfaceName memberName) { D.signalBody = [D.toVariant str] }
      D.emit dbus signal
   
  -- Workspace state colors
  , ppCurrent = \ws -> wrapClick ws $ "%{B" ++ colorAct ++ "}%{F" ++ colorBack ++ "} " ++ ws ++ " %{F-}%{B-}"
  , ppVisible = \ws -> wrapClick ws $ "%{B" ++ colorVis ++ "}%{F" ++ colorBack ++ "} " ++ ws ++ " %{F-}%{B-}"
  , ppHidden  = \ws -> wrapClick ws $ "%{B" ++ colorOcc ++ "}%{F" ++ colorVis ++ "} " ++ ws ++ " %{F-}%{B-}"
  , ppHiddenNoWindows = \ws -> wrapClick ws $ "%{F" ++ colorEmp ++ "} " ++ ws ++ " %{F-}"
   
  , ppSep     = "" 
  , ppWsSep   = ""
   
  -- ADD EXTRAS: Screen ID and Layout List
  , ppExtras  = [ logScreen, logLayouts ]
   
  -- ORDER: Only send Workspaces and Extras (the bash script processes the rest)
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
    ]