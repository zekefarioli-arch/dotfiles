#!/bin/bash
GREEN="#a6e3a1"
RED="#f38ba8"
BG="#1e1e2e"

STATUS_SCRIPT="$HOME/.config/qtile/scripts/caffeine_status.sh"

# --- Actual toggle (what really changes the state) ---
toggle() {
  if [ "$1" = "ON" ]; then
    xset s 300
    xset -dpms
  else
    xset s off
    xset +dpms
  fi
}

before="$("$STATUS_SCRIPT")"

# if it was ON, switch it OFF; if it was OFF, switch it ON
if [ "$before" = "ON" ]; then
  toggle "OFF"
else
  toggle "ON"
fi

after="$("$STATUS_SCRIPT")"

if [ "$after" = "ON" ]; then
  notify-send "  Caffeine" "Enabled" \
    -h string:x-dunst-stack-tag:caffeine \
    -h string:fgcolor:$GREEN \
    -h string:bgcolor:$BG \
    -i nf-cod-coffee
else
  notify-send "  Caffeine" "Disabled" \
    -h string:x-dunst-stack-tag:caffeine \
    -h string:bgcolor:$BG \
    -h string:fgcolor:$RED
fi
