#!/usr/bin/env bash
# XMonad autostart (Fedora). The bars are started by xmonad, not by this script.

# 1) Display and environment
[ -f ~/.xprofile ] && source ~/.xprofile
autorandr --change --default horizontal

# 2) Compositor (transparency and shadows)
pkill -x picom
picom --backend glx &

# 3) Power management and screen saver
xset s off
xset +dpms
xset dpms 300 300 300

# Lock on suspend / lid close (xidlehook is not packaged for Fedora)
pkill -x xss-lock 2>/dev/null || true
xss-lock -- ~/.local/bin/lock-screen &

xsetroot -cursor_name left_ptr

# 4) Wallpaper
# Catppuccin Mocha wallpapers from orangci/walls-catppuccin-mocha (one image
# on every monitor)
if [ -f ~/Pictures/catpuccin/dark-forest.jpg ]; then
  feh --no-fehbg --bg-fill ~/Pictures/catpuccin/dark-forest.jpg &
elif [ -x ~/.fehbg ]; then
  ~/.fehbg &
else
  xsetroot -solid '#1e1e2e'
fi

# 5) System tray applets
nm-applet &
blueman-applet &
pasystray &
copyq --start-server &
kdeconnectd &        # phone integration (KDE Connect daemon only)
dunst &
