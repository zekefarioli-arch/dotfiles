#!/bin/sh
# Arch desktop only. Run as root: sudo sh sddm-setup.sh THEME.zip [OUTPUT_TO_KEEP]
#
# 1. Installs the Catppuccin Mocha pink SDDM theme (catppuccin/sddm, Qt6) and selects it.
# 2. Shows the login screen on ONE monitor (default VGA-0, the primary): the greeter is
#    started with a DisplayCommand that switches the other outputs off. The session
#    (autostart.sh -> autorandr) turns them on again after login.
#    Why a drop-in and not an edit of /usr/share/sddm/scripts/Xsetup: the package owns that
#    file and an update would overwrite it.
# 3. Removes ble.sh (blesh), which was not useful; the .bashrc no longer loads it.
#
# Undo: delete /etc/sddm.conf.d/10-catppuccin.conf, /usr/local/share/sddm-xsetup.sh and
# /usr/share/sddm/themes/catppuccin-mocha-pink.
set -eu
zip=${1:?usage: sddm-setup.sh THEME.zip [OUTPUT]}
keep=${2:-VGA-0}

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
unzip -q "$zip" -d "$tmp"
rm -rf /usr/share/sddm/themes/catppuccin-mocha-pink
cp -r "$tmp/catppuccin-mocha-pink" /usr/share/sddm/themes/

install -d /etc/sddm.conf.d /usr/local/share
cat > /usr/local/share/sddm-xsetup.sh <<XS
#!/bin/sh
# Greeter on one monitor only; the real Xsetup of the package runs first.
[ -x /usr/share/sddm/scripts/Xsetup ] && /usr/share/sddm/scripts/Xsetup
for o in \$(xrandr --query | awk '/ connected/ {print \$1}'); do
  [ "\$o" = "$keep" ] || xrandr --output "\$o" --off
done
xrandr --output "$keep" --auto --primary
XS
chmod 755 /usr/local/share/sddm-xsetup.sh

cat > /etc/sddm.conf.d/10-catppuccin.conf <<CONF
[Theme]
Current=catppuccin-mocha-pink

[X11]
DisplayCommand=/usr/local/share/sddm-xsetup.sh
CONF

pacman -Q blesh >/dev/null 2>&1 && pacman -Rns --noconfirm blesh
echo "OK: theme installed, greeter on $keep. It shows at the next login screen (log out)."
