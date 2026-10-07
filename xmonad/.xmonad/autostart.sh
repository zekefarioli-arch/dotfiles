#!/usr/bin/env bash
# XMonad autostart (Fedora). The bars are started by xmonad, not by this script.

# 1) Display and environment
[ -f ~/.xprofile ] && source ~/.xprofile
autorandr --change --default horizontal

# 2) Compositor (transparency and shadows)
pkill -x picom
picom &                # config: ~/.config/picom/picom.conf

# 3) Power management and screen saver
xset s off
xset +dpms
xset dpms 300 300 300

# No automatic lock. xss-lock used to run lock-screen on suspend and on lid close, which
# meant a password prompt every time the lid was opened (removed on 2026-10-07 because it
# was a nuisance; see docs/DECISIONS.md). Super+L still locks by hand. To bring it back,
# uncomment the xss-lock line (and run it once in the current session).
pkill -x xss-lock 2>/dev/null || true
# xss-lock -- ~/.local/bin/lock-screen &

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
