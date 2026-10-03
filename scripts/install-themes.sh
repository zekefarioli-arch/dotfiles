#!/usr/bin/env bash
# Instala el tema Catppuccin Mocha con acento rosa (pink) para GTK, Qt,
# íconos y cursor, todo en el home del usuario (sin tocar /usr).
#
# Paquetes necesarios:
#   Fedora: sudo dnf install sassc gtk-murrine-engine kvantum qt5ct qt6ct xdg-desktop-portal-gtk unzip git curl
#   Arch:   sudo pacman -S sassc gtk-engine-murrine kvantum qt5ct qt6ct xdg-desktop-portal-gtk unzip git curl
#
# Las configuraciones (gtk-3.0, gtk-4.0, qt6ct, Kvantum, .xprofile) están en
# los paquetes de stow "gtk", "qt", "x11" y "copyq": stow --no-folding -t ~ gtk qt x11 copyq
set -euo pipefail

ACCENT=pink
FLAVOR=mocha
ICONS="$HOME/.local/share/icons"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$ICONS" "$HOME/.config/Kvantum"

echo "==> GTK: Catppuccin (Fausto-Korpsvart), acento $ACCENT, oscuro"
git clone -q --depth 1 https://github.com/Fausto-Korpsvart/Catppuccin-GTK-Theme "$TMP/gtk"
(cd "$TMP/gtk/themes" && BATCH_MODE=true ./install.sh -a "$ACCENT" -m dark >/dev/null </dev/null)
# Las apps libadwaita (GTK4) solo leen ~/.config/gtk-4.0: se enlaza el tema ahí
GTK4="$HOME/.themes/Catppuccin-${ACCENT^}-Dark/gtk-4.0"
mkdir -p "$HOME/.config/gtk-4.0"
for f in assets windows-assets gtk.css gtk-dark.css; do
  [[ -e $GTK4/$f ]] && ln -sfn "$GTK4/$f" "$HOME/.config/gtk-4.0/$f"
done

echo "==> Qt: Kvantum catppuccin-$FLAVOR-$ACCENT"
git clone -q --depth 1 https://github.com/catppuccin/kvantum "$TMP/kvantum"
rm -rf "$HOME/.config/Kvantum/catppuccin-$FLAVOR-$ACCENT"
cp -r "$TMP/kvantum/themes/catppuccin-$FLAVOR-$ACCENT" "$HOME/.config/Kvantum/"

echo "==> Íconos: Papirus-Dark con carpetas cat-$FLAVOR-$ACCENT"
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
# Cursor por defecto de X (lo usan las apps que no leen la config de GTK)
mkdir -p "$ICONS/default"
printf '[Icon Theme]\nInherits=catppuccin-%s-%s-cursors\n' "$FLAVOR" "$ACCENT" > "$ICONS/default/index.theme"

echo "==> gsettings (GTK4/libadwaita y apps que leen dconf)"
if command -v gsettings >/dev/null; then
  gsettings set org.gnome.desktop.interface gtk-theme "Catppuccin-Pink-Dark"
  gsettings set org.gnome.desktop.interface icon-theme "Papirus-Dark"
  gsettings set org.gnome.desktop.interface cursor-theme "catppuccin-$FLAVOR-$ACCENT-cursors"
  gsettings set org.gnome.desktop.interface cursor-size 24
  gsettings set org.gnome.desktop.interface color-scheme "prefer-dark"
  gsettings set org.gnome.desktop.interface font-name "Noto Sans 10"
  gsettings set org.gnome.desktop.interface monospace-font-name "JetBrainsMono Nerd Font 10"
fi



# CopyQ usa su propio sistema de temas (no el de Qt): se aplica si está corriendo
if pgrep -u "$UID" -x copyq >/dev/null; then
  "$(dirname "$0")/../copyq/apply-theme.sh"
else
  echo "CopyQ no está corriendo: aplica su tema después con copyq/apply-theme.sh"
fi
echo "Listo. Cierra sesión y vuelve a entrar para que todas las apps tomen el tema."
