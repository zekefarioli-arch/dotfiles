# Migrating the laptop from Fedora 44 to Ubuntu Server 26.04 LTS

Plan, package map and checklists for replacing Fedora on the ThinkPad X13 with
Ubuntu Server 26.04.1 LTS (text installer, "minimized"), then building the same
desktop on top of it with these dotfiles. Written on 2026-10-16, before the
reinstall. Addresses, hosts and where the backup lives are in the private repo
home-infra (`runbooks/`), because this repo is public.

Why a server install and not the desktop edition: I want the same light system I built
on Fedora, a bare console and then only what I choose (xmonad, Polybar, alacritty),
with no GNOME, no snaps pulled in by a desktop meta-package and no extra services.

## 1. Safety net, before anything is wiped

- The old 236 GB disk (Fedora as it was on 2026-10-13) is kept untouched in a USB
  enclosure. It is the way back, and a source for anything the backup missed.
- Everything that changed after 2026-10-13 is in a backup on the gateway
  (`~/migration-2026-10-16/`, mode 700): `home.tar.gz` (/home without caches),
  `etc-selected.tar.gz` (NetworkManager connections with the WireGuard key, printers,
  scanner, ssh and snapper settings), and the lists of Fedora packages.
  Verified by checksum and a listing before the disk is touched (see section 7).
- Every repo is committed and pushed. The last one that was only on this disk,
  `findajob-login`, is now a private GitLab project.
- Delete the backup from the gateway once Ubuntu works, because it holds private keys.

## 2. Install Ubuntu Server

1. Download Ubuntu Server 26.04.1 LTS from ubuntu.com/download/server (about 3 GB), check
   its SHA256 against the published value and write it to a USB stick (`dd` or Ventoy).
2. ThinkPad: F12 for the boot menu, UEFI, Secure Boot can stay on.
3. In the installer choose **Ubuntu Server (minimized)**, a wired or Wi-Fi network, the
   whole 1 TB disk, user `zeke`, a hostname of my choice (it is `fedora` today),
   **install OpenSSH server**, and no featured snaps.
4. Storage: **ext4** on the whole disk (decided on 2026-10-16: the installer does it without manual
   steps; the dotfiles and the backups cover what snapshots did on Fedora). Add the laptop to the
   nightly restic backups once Ubuntu works.

## 3. First boot

```
sudo apt update && sudo apt full-upgrade -y
sudo apt install -y git stow curl ca-certificates network-manager
# Ubuntu Server uses netplan with systemd-networkd; the laptop needs NetworkManager for Wi-Fi
# and for the wg-home connection. Set `renderer: NetworkManager` in /etc/netplan/*.yaml,
# then `sudo netplan apply`.
git clone https://github.com/zekefarioli-arch/dotfiles ~/dotfiles
```

Then restore the secrets and the repos from the backup (section 6), run the packages
below, and stow the same packages as on Fedora:
`cd ~/dotfiles && stow --no-folding -t ~ xmonad polybar bin alacritty tmux rofi dunst gtk qt x11 xdg micro picom starship nvim claude webapps copyq`
(a missing package is skipped; `bash` is stowed only if its .bashrc is wanted).

Section 4 is also `scripts/ubuntu-post-install.sh`: run it as `zeke` from the clone of dotfiles; it is
idempotent and its header lists the switches (`NM_NETPLAN`, `LAN_CIDR`, `NO_SSH_PASSWORD`,
`WITH_CHROME`, `WITH_VSCODE`, `OFFICE`, `DEV`, `REMOVE_SNAPD`), all off by default. It has been
checked with `bash -n` and against the package index, but not run on Ubuntu yet.

## 4. Packages (Fedora name to Ubuntu 26.04 name)

Every name below was checked against the Ubuntu 26.04 package index on 2026-10-16.

```
# X and the window manager. ghc and the two libghc packages are what `xmonad --recompile`
# needs to build xmonad.hs; Ubuntu has xmonad 0.18.0 and xmonad-contrib 0.18.1.
sudo apt install -y xserver-xorg xinit x11-xserver-utils x11-utils xclip xdotool \
  xserver-xephyr xvfb autorandr feh maim scrot i3lock xss-lock suckless-tools \
  xmonad xmobar ghc libghc-xmonad-dev libghc-xmonad-contrib-dev \
  polybar picom rofi dunst stalonetray lightdm lightdm-gtk-greeter

# Terminal, shell and editors
sudo apt install -y alacritty terminator tmux starship stow micro vim neovim ripgrep \
  fd-find tree jq lazygit gh tree-sitter-cli nodejs npm

# Desktop apps and themes
sudo apt install -y thunar pcmanfm mousepad atril mupdf galculator gparted simple-scan \
  flameshot copyq kdeconnect scrcpy adb libreoffice-writer libreoffice-calc \
  libreoffice-impress libreoffice-draw hunspell-es kmag zbar-tools imagemagick \
  papirus-icon-theme qt5ct qt6ct qt5-style-kvantum qt6-style-kvantum \
  gtk2-engines-murrine sassc fonts-noto-color-emoji

# Network, sound, power, hardware
sudo apt install -y network-manager-gnome blueman pipewire wireplumber pavucontrol \
  pasystray brightnessctl power-profiles-daemon fwupd smartmontools efibootmgr \
  btrfs-progs systemd-zram-generator chrony ufw plymouth

# Printing and scanning (scripts/setup-printers.sh still uses dnf and needs an apt version)
sudo apt install -y cups sane-utils sane-airscan

# Development you had on Fedora (only if still wanted)
sudo apt install -y maven openjdk-25-jdk erlang elixir
```

Differences that matter:

| On Fedora | On Ubuntu | Note |
|---|---|---|
| `dmenu`, `slock` | `suckless-tools` | one package holds both |
| `android-tools` | `adb` | |
| `chromium` | not installed | on Ubuntu it is only a snap; Brave and Chrome are enough |
| `firefox` | not installed | the Ubuntu package is a snap; use Mozilla's apt repository if it is wanted |
| `wezterm` | not installed | alacritty is the terminal; wezterm has its own apt repository if needed |
| `flatpak` | not installed | I avoid Flatpak runtimes |
| `firewalld` | `ufw` | rules in section 5 |
| `snapper` + dnf plugin | decided with the filesystem | section 2 |
| SELinux | AppArmor | nothing to configure |
| RPM Fusion codecs | `ffmpeg gstreamer1.0-plugins-bad gstreamer1.0-plugins-ugly gstreamer1.0-libav mesa-va-drivers vainfo` | check them before relying on them |

### Not in Ubuntu's archive (own repositories or downloads)

- **Brave** (`brave-browser`): Brave's apt repository, as in brave.com/linux. The `webapp`
  tool and `findajob` call `brave-browser`.
- **Google Chrome** and **VS Code**: Google's and Microsoft's apt repositories.
- **Antigravity IDE**: tarball from antigravity.google/download, extracted in
  `~/.local/opt/antigravity-ide` with a launcher in `~/.local/bin` (the Fedora RPM is
  abandoned at 1.23.2). Same steps as on 2026-10-13.
- **JetBrainsMono Nerd Font**: `scripts/install-themes.sh` downloads it; check that the script
  does not assume dnf.
- **Claude Code**, then `claude-tools` (`./install.sh`, it knows apt) and `webapps`
  (`./install.sh`, then `webapp sync`).
- **Playwright** for `findajob`: `findajob-login/install.sh` (npm, `playwright-core` only).

## 5. Settings that Fedora did for me

- **Firewall:** `sudo ufw default deny incoming`, `sudo ufw allow from <LAN> to any port 22`
  (the real LAN range is in home-infra, not here), `sudo ufw allow 1714:1764/tcp`, `sudo ufw allow 1714:1764/udp`
  for KDE Connect and `sudo ufw allow 5353/udp` for mDNS, then `sudo ufw enable`.
- **ssh:** copy the authorized key of the Arch desktop from the backup and set
  `PasswordAuthentication no` in `/etc/ssh/sshd_config.d/10-no-password.conf`.
- **Session:** LightDM must start the xmonad session: an `xmonad.desktop` in
  `/usr/share/xsessions/` or `~/.xsession`, and `lightdm` as the display manager.
- **Time zone:** the NetworkManager dispatcher script that sets Europe/London by network is in
  the backup (`/etc/NetworkManager/dispatcher.d`); check it has no Fedora-only paths.
- **Sudo without password** was a setup convenience; do not recreate it unless needed.
- **Plymouth, zram, firmware:** `fwupdmgr refresh && fwupdmgr get-updates` (the BIOS 1.54 update
  was still pending on Fedora), zram comes with `systemd-zram-generator`.

## 6. Restore from the backup

From `~/migration-2026-10-16/` on the gateway (the commands to fetch it are in home-infra):

- `home.tar.gz` into `~`: `.ssh`, `.config` (`findajob/credentials`, `mini-calendar/ics-url`,
  `printers.env`, `webapps`), `.local/share` (Brave profiles of the web apps, `webapp-find-a-job`
  with the Claude extension and the Alt+G shortcut, `calendar-app`, `copyq`), `.claude` (memory and
  sessions) and `Zeke_projects`. Check permissions afterwards: `~/.ssh` 700, keys 600.
- `etc-selected.tar.gz` as root: the NetworkManager connections (`wg-home` carries the
  WireGuard key; `chmod 600` and `nmcli connection reload`), CUPS and SANE settings.
- Re-run `npm install` in `Zeke_projects/findajob-login`, because `node_modules` was left out.

## 7. Risks to check before and after

- **picom is 12.5 on Ubuntu, 13 on Fedora.** The `rules` syntax (unified window rules) arrived
  with picom 12, so the 12.5 should accept `picom.conf`; what I could not confirm is every
  property used (the `slide-in` preset, `blur-opacity` and `shadow-opacity` inside an
  animation, `corner-radius` in a rule). Test it right after installing, before trusting the
  look: `picom --config ~/.config/picom/picom.conf --diagnostics`, then start it from a
  terminal and read its messages. Only if it rejects something, trim that line or build picom 13.
- **xmonad.hs** was written against xmonad 0.18; Ubuntu has 0.18.0 and contrib 0.18.1, so
  `xmonad --recompile` should work; confirm before trusting it.
- **Backup check, done before wiping:** compare the SHA256 computed while streaming with the one
  of the file on the gateway, list the archive (`tar -tzf`) and restore a few files to `/tmp`.
- **Local DNS:** the router's DNS sometimes returns SERVFAIL and `systemd-resolved` caches it;
  `sudo resolvectl flush-caches` fixes it (it already happened with the Find a job login).
- **Wi-Fi:** the Intel firmware comes with `linux-firmware`; confirm the card works in the
  installer, or use a wired connection (the dock) for the first boot.

## 8. After the install, check

xmonad starts from LightDM; both bars and the pill shape; Super+Ctrl+T dropdown; rofi; dunst;
`nvim` with LazyVim; alacritty and tmux; Wi-Fi and `home-vpn` (Super+O); KDE Connect; `findajob`
with Alt+G; the printers; `git pull` in `~/dotfiles` and `claude-fresh`; a nightly backup of
this laptop if one is set up; and update `docs/STATE.md` for the new system (this file stays
as the record of the migration).

## Limits and next steps

`scripts/ubuntu-post-install.sh` exists but has not been run on a real Ubuntu install, so the
first run is the real test; read its output rather than trusting it. The filesystem is decided
(ext4) and the picom question is answered by testing right after the install. Still open: a
`system/` file for the apt snapshot hook if I ever move to btrfs, and an Ubuntu version of
`scripts/setup-printers.sh`.
