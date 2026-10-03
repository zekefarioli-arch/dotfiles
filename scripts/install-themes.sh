#!/usr/bin/env bash
# Install the Catppuccin Mocha theme with the pink accent for GTK, Qt,
# icons and cursor, all inside the user's home (without touching /usr).
#
# Required packages:
#   Fedora: sudo dnf install sassc gtk-murrine-engine kvantum qt5ct qt6ct xdg-desktop-portal-gtk unzip git curl
#   Arch:   sudo pacman -S sassc gtk-engine-murrine kvantum qt5ct qt6ct xdg-desktop-portal-gtk unzip git curl
#
# The configs (gtk-3.0, gtk-4.0, qt6ct, Kvantum, .xprofile) live in the
# stow packages "gtk", "qt", "x11" and "copyq": stow --no-folding -t ~ gtk qt x11 copyq
set -euo pipefail

ACCENT=pink
FLAVOR=mocha
ICONS="$HOME/.local/share/icons"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$ICONS" "$HOME/.config/Kvantum"

echo "==> GTK: Catppuccin (Fausto-Korpsvart), accent $ACCENT, dark"
git clone -q --depth 1 https://github.com/Fausto-Korpsvart/Catppuccin-GTK-Theme "$TMP/gtk"
(cd "$TMP/gtk/themes" && BATCH_MODE=true ./install.sh -a "$ACCENT" -m dark >/dev/null </dev/null)
# libadwaita (GTK4) apps only read ~/.config/gtk-4.0, so the theme is linked there
GTK4="$HOME/.themes/Catppuccin-${ACCENT^}-Dark/gtk-4.0"
mkdir -p "$HOME/.config/gtk-4.0"
for f in assets windows-assets gtk.css gtk-dark.css; do
  [[ -e $GTK4/$f ]] && ln -sfn "$GTK4/$f" "$HOME/.config/gtk-4.0/$f"
done

echo "==> Qt: Kvantum catppuccin-$FLAVOR-$ACCENT"
git clone -q --depth 1 https://github.com/catppuccin/kvantum "$TMP/kvantum"
rm -rf "$HOME/.config/Kvantum/catppuccin-$FLAVOR-$ACCENT"
cp -r "$TMP/kvantum/themes/catppuccin-$FLAVOR-$ACCENT" "$HOME/.config/Kvantum/"

echo "==> Icons: Papirus-Dark with cat-$FLAVOR-$ACCENT folders"
curl -fsSL https://raw.githubusercontent.com/PapirusDevelopmentTeam/papirus-icon-theme/master/install.sh |
  DESTDIR="$ICONS" sh >/dev/null
git clone -q --depth 1 https://github.com/catppuccin/papirus-folders "$TMP/folders"
cp -r "$TMP/folders/src/"* "$ICONS/Papirus/"
curl -fsSL https://raw.githubusercontent.com/PapirusDevelopmentTeam/papirus-folders/master/papirus-folders \
  -o "$TMP/papirus-folders"
bash "$TMP/papirus-folders" -C "cat-$FLAVOR-$ACCENT" --theme Papirus-Dark >/dev/null

echo "==> Cursor: catppuccin-$FLAVOR-$ACCENT-cursors"
curl -fsSL "https://github.com/catppuccin/cursors/releases/latest/download/catppuccin-$FLAVOR-$ACCENT-cursors.zip" \
  -o "$TMP/cursors.zip"
rm -rf "$ICONS/catppuccin-$FLAVOR-$ACCENT-cursors"
unzip -q "$TMP/cursors.zip" -d "$ICONS"
# Default X cursor (for apps that do not read the GTK settings)
mkdir -p "$ICONS/default"
printf '[Icon Theme]\nInherits=catppuccin-%s-%s-cursors\n' "$FLAVOR" "$ACCENT" > "$ICONS/default/index.theme"

echo "==> gsettings (GTK4/libadwaita and apps that read dconf)"
if command -v gsettings >/dev/null; then
  gsettings set org.gnome.desktop.interface gtk-theme "Catppuccin-Pink-Dark"
  gsettings set org.gnome.desktop.interface icon-theme "Papirus-Dark"
  gsettings set org.gnome.desktop.interface cursor-theme "catppuccin-$FLAVOR-$ACCENT-cursors"
  gsettings set org.gnome.desktop.interface cursor-size 24
  gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"
  gsettings set org.gnome.desktop.interface font-name "Noto Sans 10"
  gsettings set org.gnome.desktop.interface monospace-font-name "JetBrainsMono Nerd Font 10"
fi



# CopyQ has its own theme system (not Qt's): apply it if CopyQ is running
if pgrep -u "$UID" -x copyq >/dev/null; then
  "$(dirname "$0")/../copyq/apply-theme.sh"
else
  echo "CopyQ is not running: apply its theme later with copyq/apply-theme.sh"
fi
echo "Done. Log out and back in so every app picks up the theme."
