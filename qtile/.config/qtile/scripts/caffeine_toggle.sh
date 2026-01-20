#!/bin/bash
GREEN="#a6e3a1"
RED="#f38ba8"
BG="#1e1e2e"

STATUS_SCRIPT="$HOME/.config/qtile/scripts/caffeine_status.sh"

# --- Toggle real (lo que realmente cambia el estado) ---
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

# si estaba ON, lo pasamos a OFF; si estaba OFF, lo pasamos a ON
if [ "$before" = "ON" ]; then
  toggle "OFF"
else
  toggle "ON"
fi

after="$("$STATUS_SCRIPT")"

if [ "$after" = "ON" ]; then
  notify-send "  Caffeine" "Activado" \
    -h string:x-dunst-stack-tag:caffeine \
    -h string:fgcolor:$GREEN \
    -h string:bgcolor:$BG \
    -i nf-cod-coffee
else
  notify-send "  Caffeine" "Desactivado" \
    -h string:x-dunst-stack-tag:caffeine \
    -h string:bgcolor:$BG \
    -h string:fgcolor:$RED
fi
