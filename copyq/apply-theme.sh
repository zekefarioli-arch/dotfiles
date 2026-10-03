#!/usr/bin/env bash
# Aplica a CopyQ el tema Catppuccin Mocha Pink y oculta la barra de menú.
# Requiere el servidor de CopyQ corriendo (lo lanza autostart.sh).
# Las opciones del menú siguen en el click derecho y en el ícono del tray.
set -euo pipefail
copyq loadTheme "$HOME/.config/copyq/themes/catppuccin-mocha-pink.ini"
copyq config native_menu_bar false >/dev/null
