#!/usr/bin/env bash
# Apply the Catppuccin Mocha Pink theme to CopyQ and hide the menu bar.
# Requires the CopyQ server to be running (autostart.sh starts it).
# The menu options are still available on right click and in the tray icon.
set -euo pipefail
copyq loadTheme "$HOME/.config/copyq/themes/catppuccin-mocha-pink.ini"
copyq config native_menu_bar false >/dev/null
