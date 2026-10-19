#!/usr/bin/env bash
# Ubuntu Server 26.04 LTS (minimized) post-install setup for this laptop (ThinkPad X13 Gen 1
# AMD): the same light desktop as the Fedora one (xmonad, Polybar, picom, rofi, dunst,
# alacritty). Plan, package map and checklists: docs/UBUNTU-MIGRATION.md.
# Run it as zeke (not root; it uses sudo), from a clone of dotfiles, after the first boot.
# Idempotent: safe to run again. Package names were checked against the Ubuntu 26.04 index.
#
# Switches (environment variables), all off by default because each can cut the network or
# lock me out, or is heavy:
#   NM_NETPLAN=1        let NetworkManager manage the network (Ubuntu Server uses systemd-networkd).
#                       Needed for Wi-Fi and the wg-home VPN. Moves the netplan files aside and turns
#                       systemd-networkd off; REBOOT to apply it. `netplan apply` with both daemons
#                       running left two wpa_supplicants fighting and the laptop off the network (2026-10-17).
#   LAN_CIDR=x.x.x.x/24 turn on ufw: deny incoming, allow ssh from that range only, allow KDE Connect.
#                       The real range is in the private repo home-infra.
#   NO_SSH_PASSWORD=1   turn off ssh passwords (only if ~/.ssh/authorized_keys has a key).
#   WITH_CHROME=1       Google Chrome from Google's apt repository.
#   WITH_VSCODE=1       VS Code from Microsoft's apt repository.
#   OFFICE=1            LibreOffice (large).
#   DEV=1               maven, OpenJDK, Erlang and Elixir, as on Fedora.
#   REMOVE_SNAPD=1      purge snapd (the minimized server may still have it; I avoid snaps).
set -euo pipefail
DOTFILES="$(cd "$(dirname "$0")/.." && pwd)"
[[ $EUID -ne 0 ]] || { echo "run this as your own user, not root (it calls sudo itself)"; exit 1; }
[[ -r /etc/os-release ]] && . /etc/os-release
[[ ${ID:-} == ubuntu ]] || { echo "this is for Ubuntu (found: ${ID:-unknown})"; exit 1; }
export DEBIAN_FRONTEND=noninteractive
apt_install() { sudo apt-get install -y --no-install-recommends "$@"; }

echo "==> apt: update and upgrade"
sudo apt-get update
sudo apt-get full-upgrade -y

echo "==> Base tools"
apt_install ca-certificates curl wget gpg unzip git stow jq openssh-server \
  software-properties-common xdg-utils xdg-user-dirs locales

echo "==> X, window manager and bar"
# ghc and the two libghc packages are what `xmonad --recompile` needs to build xmonad.hs
apt_install xserver-xorg xinit x11-xserver-utils x11-utils xclip xdotool xserver-xephyr xvfb \
  autorandr feh maim scrot i3lock xss-lock suckless-tools \
  xmonad xmobar ghc libghc-xmonad-dev libghc-xmonad-contrib-dev \
  polybar picom rofi dunst stalonetray lightdm lightdm-gtk-greeter libnotify-bin

echo "==> Terminal, shell and editors"
apt_install alacritty terminator tmux starship micro vim neovim ripgrep fd-find tree \
  lazygit gh tree-sitter-cli nodejs npm

echo "==> Desktop apps, themes and fonts"
apt_install thunar pcmanfm mousepad atril mupdf galculator gparted simple-scan flameshot \
  copyq copyq-plugins kdeconnect scrcpy adb kmag zbar-tools imagemagick gvfs \
  python3 python3-gi gir1.2-gtk-3.0 python3-icalendar python3-dateutil \
  papirus-icon-theme qt5ct qt6ct qt5-style-kvantum qt6-style-kvantum \
  gtk2-engines-murrine sassc xdg-desktop-portal-gtk dconf-cli libglib2.0-bin \
  fonts-noto-core fonts-noto-color-emoji fonts-dejavu-mono hunspell-es

echo "==> Network, sound, power, hardware"
apt_install network-manager network-manager-gnome blueman pipewire pipewire-pulse wireplumber \
  pulseaudio-utils pavucontrol pasystray brightnessctl power-profiles-daemon fwupd \
  smartmontools efibootmgr btrfs-progs systemd-zram-generator chrony plymouth \
  amd64-microcode linux-firmware
# The backlight file is writable by group video only: without this the brightness keys
# (osd-brightness -> brightnessctl) fail with 'Permission denied'. Takes effect at next login.
sudo usermod -aG video "$USER"

echo "==> Printing and scanning (queues: scripts/setup-printers.sh, still written for dnf)"
apt_install cups sane-utils sane-airscan

echo "==> Video codecs (ffmpeg, GStreamer, VA-API for the AMD GPU)"
apt_install ffmpeg gstreamer1.0-plugins-good gstreamer1.0-plugins-bad gstreamer1.0-plugins-ugly \
  gstreamer1.0-libav mesa-va-drivers vainfo

if [[ ${OFFICE:-0} == 1 ]]; then
  echo "==> LibreOffice"
  apt_install libreoffice-writer libreoffice-calc libreoffice-impress libreoffice-draw
fi
if [[ ${DEV:-0} == 1 ]]; then
  echo "==> Development: maven, OpenJDK, Erlang, Elixir"
  apt_install maven openjdk-25-jdk erlang elixir
fi

echo "==> Brave (its own apt repository; webapp and findajob call brave-browser)"
sudo curl -fsSLo /usr/share/keyrings/brave-browser-archive-keyring.gpg \
  https://brave-browser-apt-release.s3.brave.com/brave-browser-archive-keyring.gpg
echo "deb [signed-by=/usr/share/keyrings/brave-browser-archive-keyring.gpg] https://brave-browser-apt-release.s3.brave.com/ stable main" |
  sudo tee /etc/apt/sources.list.d/brave-browser-release.list >/dev/null
sudo apt-get update
apt_install brave-browser

# Claude Desktop (Linux beta): Anthropic's own apt repository, key fingerprint
# 31DDDE24DDFAB679F42D7BD2BAA929FF1A7ECACE. apt_install passes --no-install-recommends on purpose: the
# recommends would pull 55 packages; only QEMU/OVMF/virtiofsd (Cowork tab, and the CI VMs) are
# added by hand, gnome-keyring is not; see DECISIONS.md.
echo "==> Claude Desktop"
sudo curl -fsSLo /usr/share/keyrings/claude-desktop-archive-keyring.asc https://downloads.claude.ai/claude-desktop/key.asc
echo "deb [arch=amd64,arm64 signed-by=/usr/share/keyrings/claude-desktop-archive-keyring.asc] https://downloads.claude.ai/claude-desktop/apt/stable stable main" |
  sudo tee /etc/apt/sources.list.d/claude-desktop.list >/dev/null
sudo apt-get update
apt_install claude-desktop socat qemu-system-x86 ovmf virtiofsd
sudo usermod -aG kvm "$USER"  # Cowork also needs /dev/vhost-vsock; log out and in once

if [[ ${WITH_CHROME:-0} == 1 ]]; then
  echo "==> Google Chrome"
  curl -fsSL https://dl.google.com/linux/linux_signing_key.pub |
    sudo gpg --yes --dearmor -o /usr/share/keyrings/google-chrome.gpg
  # Same file name the package itself uses, so it does not add a second, duplicate source
  echo "deb [arch=amd64 signed-by=/usr/share/keyrings/google-chrome.gpg] https://dl.google.com/linux/chrome/deb/ stable main" |
    sudo tee /etc/apt/sources.list.d/google-chrome.list >/dev/null
  sudo apt-get update
  apt_install google-chrome-stable
fi
if [[ ${WITH_VSCODE:-0} == 1 ]]; then
  echo "==> VS Code"
  curl -fsSL https://packages.microsoft.com/keys/microsoft.asc |
    sudo gpg --yes --dearmor -o /usr/share/keyrings/microsoft.gpg
  echo "deb [arch=amd64 signed-by=/usr/share/keyrings/microsoft.gpg] https://packages.microsoft.com/repos/code stable main" |
    sudo tee /etc/apt/sources.list.d/vscode.list >/dev/null
  sudo apt-get update
  apt_install code
fi

echo "==> Locale en_GB.UTF-8 (as the Fedora langpacks-en_GB)"
sudo locale-gen en_GB.UTF-8 >/dev/null
sudo update-locale LANG=en_GB.UTF-8

echo "==> zram swap (Fedora's default: half of the RAM, at most 8 GiB)"
sudo install -d /etc/systemd
printf '[zram0]\nzram-size = min(ram / 2, 8192)\n' | sudo tee /etc/systemd/zram-generator.conf >/dev/null

echo "==> Power profiles (balanced / power-saver / performance)"
sudo systemctl enable --now power-profiles-daemon

echo "==> Login screen: LightDM with the Catppuccin greeter, session xmonad"
sudo install -D -m 644 "$DOTFILES/system/etc/lightdm/lightdm-gtk-greeter.conf" /etc/lightdm/lightdm-gtk-greeter.conf
# Flat popup menus in the greeter (no compositor there: round corners show square edges)
sudo install -D -o lightdm -g lightdm -m 644 "$DOTFILES/system/etc/lightdm/greeter-gtk.css" /var/lib/lightdm/.config/gtk-3.0/gtk.css
[[ -f ~/Pictures/catpuccin/kitty.jpg ]] &&
  sudo install -D -m 644 ~/Pictures/catpuccin/kitty.jpg /usr/share/backgrounds/login-kitty.jpg
echo /usr/sbin/lightdm | sudo tee /etc/X11/default-display-manager >/dev/null
# The greeter runs as user lightdm and cannot read ~/.themes or ~/.local/share/icons, so the
# theme and cursor must also be in /usr/share (run after scripts/install-themes.sh).
for d in "$HOME/.themes/Catppuccin-Pink-Dark:/usr/share/themes" \
         "$HOME/.local/share/icons/catppuccin-mocha-pink-cursors:/usr/share/icons"; do
  [[ -d ${d%%:*} ]] && sudo cp -rL "${d%%:*}" "${d##*:}/" && sudo chmod -R a+rX "${d##*:}/$(basename "${d%%:*}")"
done
sudo systemctl enable lightdm
if [[ ! -e /usr/share/xsessions/xmonad.desktop ]]; then
  printf '[Desktop Entry]\nName=xmonad\nComment=Tiling window manager\nExec=xmonad\nType=Application\n' |
    sudo tee /usr/share/xsessions/xmonad.desktop >/dev/null
fi
# A graphical target is needed for the display manager on a server install
sudo systemctl set-default graphical.target

echo "==> Time zone by network (NetworkManager dispatcher, same script as on Fedora)"
sudo install -D -m 755 "$DOTFILES/system/etc/NetworkManager/dispatcher.d/90-auto-timezone" \
  /etc/NetworkManager/dispatcher.d/90-auto-timezone

if [[ ${REMOVE_SNAPD:-0} == 1 ]]; then
  echo "==> Removing snapd"
  sudo apt-get purge -y snapd
  sudo apt-mark hold snapd >/dev/null
fi

if [[ ${NM_NETPLAN:-0} == 1 ]]; then
  echo "==> NetworkManager manages the network"
  sudo mkdir -p /etc/netplan.pre-nm
  for f in /etc/netplan/*.yaml; do [[ -e $f ]] && sudo mv "$f" /etc/netplan.pre-nm/; done
  printf 'network:\n  version: 2\n  renderer: NetworkManager\n' | sudo tee /etc/netplan/01-network-manager.yaml >/dev/null
  sudo chmod 600 /etc/netplan/01-network-manager.yaml
  # cloud-init would write the old network config back at the next boot
  [[ -d /etc/cloud/cloud.cfg.d ]] &&
    echo 'network: {config: disabled}' | sudo tee /etc/cloud/cloud.cfg.d/99-disable-network-config.cfg >/dev/null
  sudo systemctl enable NetworkManager
  sudo systemctl disable systemd-networkd systemd-networkd.socket
  echo "    Reboot to apply it (not netplan apply). Restore the NM Wi-Fi profile first, or the Wi-Fi will not come back."
  echo "    After restoring Brave profiles, delete their Singleton* links or the browser waits forever." 
  echo "    To undo: put the files of /etc/netplan.pre-nm back and remove 01-network-manager.yaml"
fi

if [[ -n ${LAN_CIDR:-} ]]; then
  echo "==> ufw: deny incoming; ssh from $LAN_CIDR only; KDE Connect"
  apt_install ufw
  sudo ufw default deny incoming
  sudo ufw default allow outgoing
  sudo ufw allow from "$LAN_CIDR" to any port 22 proto tcp
  sudo ufw allow 1714:1764/tcp
  sudo ufw allow 1714:1764/udp
  sudo ufw allow 5353/udp
  sudo ufw --force enable
fi

if [[ ${NO_SSH_PASSWORD:-0} == 1 ]]; then
  if [[ -s $HOME/.ssh/authorized_keys ]]; then
    echo "==> ssh: passwords off (keys only)"
    echo 'PasswordAuthentication no' | sudo tee /etc/ssh/sshd_config.d/10-no-password.conf >/dev/null
    sudo sshd -t && sudo systemctl reload ssh
  else
    echo "==> ssh: NOT turning passwords off, ~/.ssh/authorized_keys has no key (you would be locked out)"
  fi
fi

echo "==> Firmware updates available (apply with: sudo fwupdmgr update)"
sudo fwupdmgr refresh --force >/dev/null 2>&1 || true
sudo fwupdmgr get-updates || true

cat <<'EOF'

Done. What is left (details in docs/UBUNTU-MIGRATION.md):
  1. Restore the backup (ssh keys, ~/.config secrets, browser profiles, Zeke_projects, wg-home).
  2. cd ~/dotfiles && stow --no-folding -t ~ xmonad polybar bin alacritty tmux rofi dunst gtk qt x11 xdg micro picom starship nvim claude webapps copyq
  3. scripts/install-themes.sh (GTK, Kvantum, icons, cursor) and the JetBrainsMono Nerd Font.
  4. claude-tools and webapps: ./install.sh in each; then `webapp sync`; Claude Code; findajob-login/install.sh.
  5. Antigravity IDE from antigravity.google/download (tarball in ~/.local/opt).
  6. xmonad --recompile, picom --diagnostics, reboot, and pick the xmonad session in LightDM.
EOF
