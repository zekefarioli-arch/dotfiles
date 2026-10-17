# System state: ThinkPad X13

The current state of the ThinkPad: what is installed and configured, what is
pending and what is known to be broken. Claude Code reads this at the start of
every session on this machine, through the symlink ~/.claude/STATE.local.md; keep
it short and true, and update it in the same commit as the change it describes.
History goes in CHANGELOG.md, reasons in DECISIONS.md. STATE.md is the Ryzen 9
desktop's file and describes the shared desktop (xmonad, Polybar, picom, themes,
web apps, Claude tools) in detail; STATE-arch.md is the Arch desktop's.

_Last updated: 2026-10-17_

## Machine
- Lenovo ThinkPad X13 Gen 1 AMD (20UGS2Q500), 30 GiB RAM, 256 GB NVMe (WDC SN730,
  236 GB usable). BIOS R1CET79W (1.48).
- Fedora 44, kernel 7.2.x, btrfs root with snapper snapshots. This is the original
  Fedora disk: from 2026-10-13 the laptop ran Ubuntu 26.04 on a 1 TB disk, which moved
  to the Ryzen 9 desktop on 2026-10-17 (see STATE.md and docs/UBUNTU-MIGRATION.md), and
  the 256 GB disk went back into the laptop.
- Hostname `zeke-thinkpad-x13` (was `fedora`, renamed 2026-10-17). Fixed address by a DHCP
  reservation by MAC in the home router; the address is in home-infra, not here (this repo
  is public). The Wi-Fi MAC is random per network (NetworkManager `stable-ssid`): after a
  reinstall it probably changes, and then the MAC in the router's reservation line must
  change too (runbook dhcp-reservation in home-infra).
- Wi-Fi only (`wlp3s0`, CASITA_5G); timezone Europe/London, set automatically by a
  NetworkManager dispatcher script.
- User `zeke`; LightDM starts the xmonad session.

## Differences from the Ryzen 9 desktop (read before assuming)
- Package manager: dnf (Fedora), not apt. The package map is in docs/UBUNTU-MIGRATION.md.
- Battery and brightness keys exist here (the Ryzen has neither).
- Printing and scanning are set up here (CUPS with the Brother and the Canon, sane-airscan;
  `scripts/setup-printers.sh`, addresses in ~/.config/printers.env, not in the repo).
- VPN to the house: NetworkManager connection `wg-home` (WireGuard, split tunnel, no
  autoconnect), toggled with Super+O (`home-vpn`).
- Code projects live in ~/Zeke_projects.
- Alacritty font size is 11.0, set in ~/.config/alacritty/local.toml (not in the repo; the shared
  alacritty.toml imports it). Do not put `size` back in the shared file: it would win over this one.
- The `machine` tool (bin/.local/bin/machine) is in the repo but not installed here yet: it
  needs `stow --no-folding -t ~ bin` and the link ~/.config/machine/network.conf to
  home-infra/config/machine-network.conf.
- Git has no global identity on this laptop: commits need `-c user.name=... -c user.email=...`
  until it is set.

## Pending
- Ubuntu again on this laptop in a few days (2026-10-17): follow docs/UBUNTU-MIGRATION.md;
  when it is done, replace this file's Machine section.
- Firmware: BIOS 1.54 and Secure Boot KEK/UEFI CA 2023 plus dbx updates are
  available. Needs the charger plugged in: `sudo fwupdmgr update`.
- Passwordless sudo (/etc/sudoers.d/zeke-nopasswd) was added for the setup; remove it when
  the setup is done.
- Arch and Ryzen 9 keys: this laptop's key is authorised on the Ryzen 9 (2026-10-17);
  the dedicated Claude key is not, on purpose.
- Menus: the picom catch-all rule (no blur on translucent windows) and the flat GTK menus
  (gtk.css) are not confirmed on screen; open Brave's menu and Mousepad's File menu.
- Web apps: Google Messages needs its Claude extension (`webapp extensions google-messages`)
  and the phone pairing again; FireShot is not installed in the shared profile yet.
- Polybar hot-plugging was only tested with simulated monitors; confirm with a real
  external monitor.

## Known issues
- picom died once without a trace; if blur or the dropdown animation stop working, check
  `pgrep -x picom` and start it with `picom &`.
- A web app window opened before an `xmonad --restart` can stay unmanaged; close it and
  open it again from rofi.
- The dropdown terminal does not hide by itself when it loses the focus; it hides on the
  shortcuts listed in STATE.md.
- The local DNS (router) fails to resolve some hosts, such as download.brother.com;
  resolve through another DNS if a download fails.
