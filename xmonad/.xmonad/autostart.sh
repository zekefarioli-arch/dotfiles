#!/usr/bin/env bash

# 1) Configuración de pantalla y entorno
source ~/.xprofile
autorandr --load 2monitors

# 2) Compositor (Transparencias y sombras)
# Matamos instancias previas para evitar duplicados al recargar
pkill picom
picom &

# 3) Configuración de energía y protector de pantalla
xset s off
xset +dpms
xset dpms 300 300 300

# Limpieza de procesos de bloqueo anteriores
pkill -x xss-lock 2>/dev/null || true
pkill -x xidlehook 2>/dev/null || true

# Lock tras 4 minutos
xidlehook \
  # --not-when-fullscreen \
  # --timer 600 'i3lock-fancy' '' &

pkill polybar
sleep 1
~/.config/polybar/launch-polybar.sh &
# 4) Fondo de pantalla (Solo una vez es necesario)
feh --no-fehbg --bg-fill ~/Pictures/catpuccin/city-horizon.jpg ~/Pictures/catpuccin/flower.jpg &



# 6) Applets del Systray
nm-applet &
blueman-applet &
pasystray &
copyq --start-server &
dunst &
