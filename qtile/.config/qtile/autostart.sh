#!/usr/bin/env bash
source ~/.xprofile
autorandr --load 2monitors
feh --no-fehbg --bg-fill ~/Pictures/catpuccin/bluehour.jpg ~/Pictures/catpuccin/Gemini_Generated_Image_trjufdtrjufdtrju.png

# 3) Compositor
picom &

#!/bin/sh

# Enable DPMS (optional but recommended)
xset s off
xset +dpms
xset dpms 300 300 300

# Avoid duplicate lockers
pkill -x xss-lock 2>/dev/null || true
pkill -x xidlehook 2>/dev/null || true

# Lock after 4 minutes of inactivity
xidlehook \
  # --not-when-fullscreen \
  # --timer 240 'i3lock-fancy' '' &

# 4) Wallpaper
feh --bg-fill ~/Pictures/wallpaper.jpg &

# 5) Network / Bluetooth
nm-applet &
blueman-applet &

# 6) Audio tray
pasystray &

# 7) Clipboard (Win+V)
copyq --start-server &

# 8) Notifications
dunst &