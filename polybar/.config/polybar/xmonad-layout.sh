#!/bin/bash

# 1. Capture the Monitor ID (0 or 1) passed from Polybar
MY_ID=$1

if [ -z "$MY_ID" ]; then
  echo "ERR"
  exit 1
fi

# === FIX FOR SWAPPED LAYOUTS ===
# We calculate the "Opposite ID" just for the layout icon.
# If MY_ID is 0, we look for Layout 1.
# If MY_ID is 1, we look for Layout 0.
if [ "$MY_ID" == "0" ]; then
    LAYOUT_ID="1"
else
    LAYOUT_ID="0"
fi

# Colors (Catppuccin Mocha)
COLOR_ACT="#f5c2e7" # Pink (Active/Focused Monitor)
COLOR_INA="#1e1e2e" # Dark/Background (Inactive Monitor) - Blends with bar background
COLOR_TXT="#6c7086" # Grey Text

# Listen to DBus signals from XMonad
dbus-monitor "interface='org.xmonad.Log',member='Update'" | \
sed -u -n 's/^[ \t]*string "\(.*\)"/\1/p' | \
while read -r line; do
    
    # 2. EXTRACT LAYOUT (USING THE INVERTED ID)
    # We use LAYOUT_ID here because your icons are crossed.
    # grep extracts the match, cut takes the word after the colon
    RAW_LAYOUT=$(echo "$line" | grep -o "LAY${LAYOUT_ID}:[^ ]*" | cut -d':' -f2)

    # If empty (at startup), set a default
    if [ -z "$RAW_LAYOUT" ]; then RAW_LAYOUT="..."; fi

    # 3. CONVERT TEXT TO ICONS (Optional)
    case "$RAW_LAYOUT" in
        *"Tall"*)      ICON_LAY=" " ;;  # Tiled/Columns
        *"Full"*)      ICON_LAY=" " ;;  # Fullscreen
        *"Grid"*)      ICON_LAY=" " ;;  # Grid
        *"ThreeCol"*)  ICON_LAY=" " ;;  # Three Columns
        *"Mirror"*)    ICON_LAY=" " ;;  # Mirror/Stack
        *)             ICON_LAY="$RAW_LAYOUT" ;; # Fallback to text if unknown
    esac

    # 4. FINAL CLEANUP
    # Remove technical tags (SCREEN:..., LAY0:..., LAY1:...)
    # ensuring only the workspace list remains in $CONTENT
    CONTENT=$(echo "$line" | sed 's/SCREEN:.*//; s/LAY[0-9]:[^ ]*//g')

    # 5. ACTIVE MONITOR LOGIC (USING THE ORIGINAL ID)
    # We use MY_ID here because you confirmed this part works correctly for you.
    
    if [[ "$line" == *"SCREEN:$MY_ID"* ]]; then
        # === LOGIC PRESERVED AS REQUESTED ===
        # Based on your previous message, this order works for your setup:
        # If SCREEN match found -> Show INACTIVE style
        echo "%{B$COLOR_INA}%{F$COLOR_TXT}  󰶐   %{F-}%{B-} $CONTENT %{F$COLOR_TXT}|%{F-} $ICON_LAY"
    else
        # If SCREEN match NOT found -> Show ACTIVE style
        echo "%{B$COLOR_ACT}%{F#1e1e2e}  󰍹   %{F-}%{B-} $CONTENT %{F$COLOR_TXT}|%{F-} $ICON_LAY"
    fi
done