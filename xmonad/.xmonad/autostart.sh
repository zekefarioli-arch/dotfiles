#!/usr/bin/env bash
# Autostart de XMonad (Fedora). Las barras las lanza xmonad, no este script.

# 1) Configuración de pantalla y entorno
[ -f ~/.xprofile ] && source ~/.xprofile
autorandr --change --default horizontal

# 2) Compositor (Transparencias y sombras)
pkill -x picom
picom --backend glx &

# 3) Configuración de energía y protector de pantalla
xset s off
xset +dpms
xset dpms 300 300 300

# Bloqueo al suspender / cerrar la tapa (xidlehook no está en Fedora)
pkill -x xss-lock 2>/dev/null || true
xss-lock -- i3lock -n -c 1e1e2e &

xsetroot -cursor_name left_ptr

# 4) Fondo de pantalla
if [ -f ~/Pictures/catpuccin/city-horizon.jpg ]; then
  feh --no-fehbg --bg-fill ~/Pictures/catpuccin/city-horizon.jpg ~/Pictures/catpuccin/flower.jpg &
elif [ -x ~/.fehbg ]; then
  ~/.fehbg &
else
  xsetroot -solid '#1e1e2e'
fi

# 5) Applets del Systray
nm-applet &
blueman-applet &
pasystray &
copyq --start-server &
dunst &
