# System state

The current state of this machine (the Ryzen 9 desktop): what is installed and configured, what is
pending and what is known to be broken. Claude Code reads this at the start of
every session; keep it short and true, and update it in the same commit as
the change it describes. History goes in CHANGELOG.md, reasons in
DECISIONS.md.

_Last updated: 2026-10-29_

## Machine
- Ryzen 9 desktop (the 1 TB NVMe, Crucial P310, moved here from the ThinkPad X13 on 2026-10-17; it was the laptop's disk since
  2026-10-13). Hostname `zeke-ryzen9-desktop`. The ThinkPad no longer has this system. 32 GB of RAM since the upgrade (2 x 16 GB Patriot DDR5, "6000 Series", dual channel; EXPO/XMP on, running at 6000 MT/s, checked
  2026-10-21 with `sudo dmidecode -t memory`; 2 of the 4 slots are free). KVM works (AMD-V, `kvm_amd` loaded). Other hardware details: not written down yet.
- Ubuntu 26.04 LTS (server install, minimized, plus the desktop of these dotfiles), kernel 7.0.x, ext4 on LVM (the root volume is 300 GB of the 931 GB disk, extended from the installer's 100 GB on 2026-10-18; about 628 GB stay unassigned in the volume group `ubuntu-vg` for later: `sudo lvextend -r -L +NG /dev/ubuntu-vg/ubuntu-lv`)
  (replaced Fedora 44 on 2026-10-17; plan and package map in docs/UBUNTU-MIGRATION.md; the old Fedora
  disk is kept in a USB enclosure). Hostname `zeke-ryzen9-desktop` (was `zeke-thinkpad`); ufw on (ssh from the LAN only).
- Wired (`eno1`, MAC 30:c5:99:d2:3b:ee), managed by NetworkManager through netplan; timezone Europe/London, set
  automatically by a NetworkManager dispatcher script. Fixed address `.23` (a DHCP reservation by this MAC in the router, outside
  the pool; made and checked on 2026-10-17). Its MAC is in the NestNetController group `workstations` (protected) on `hs-gateway` since 2026-10-20, so the night cut of the house internet never takes it (tested with only `essential` on and open mode off). The Wi-Fi card (`wlp8s0`) is down;
  the netplan file for `CASITA_5G` is left over from the laptop (it matches `wlp3s0`) and does nothing here.
- User `zeke`; LightDM starts the xmonad session.
- Virtualization (2026-10-17): KVM and libvirt (`qemu:///system`, packages without recommends; user `zeke` is in `libvirt` and `kvm`).
  One VM, `ci-runner`: Ubuntu Server 26.04.1, 6 vCPU, 6 GB, 80 GB, UEFI, CPU shares 256, autostart. It hosts a persistent GitHub Actions
  runners for the private repos `mock-oclock` and `due-card` (one runner each, label `ryzen-vm`; the repository variable `CI_RUNS_ON` of each
  switches its CI between the VM and GitHub's runners; a lock makes the two repos share the VM one job at a time). Two password-protected web panels for it (the runner's current step, the VM's load) are served by nginx on this
  desktop and reachable from the home network only (ufw). The VM may use 6 GB of the 30 GiB of RAM and gives back what it frees (free page reporting on its memory balloon, 2026-10-17): idle it holds about 1.6 GiB of the host, not 5.7; with the 32 GB now installed it is comfortable. Runbook and addresses: home-infra
  `runbooks/ci-runner.md` (private repo). The suspend setting must stay off while it serves CI.
  Memory back to the host (2026-10-18): inside the VM a timer (`ci-trim-memory.timer`, every minute; files in `system/ci-vm/`, installed in the VM as `/usr/local/sbin/ci-trim-memory.sh` and `/etc/systemd/system/ci-trim-memory.{service,timer}`) drops the disk cache and compacts the memory so the free page reporting can return it; it only acts when no `Runner.Worker` runs, the 1-minute load is under 0.30 (about 2 minutes after the last job) and there are 300 MiB of cache or more; it is silent unless it trims. See what it did with `ssh ci@<vm> sudo journalctl -t ci-trim-memory` (works because the VM user `ci` has `NOPASSWD:ALL`, the cloud-init default; `ci-status` also needs `sudo -n` for the trim script); `ci-status` (Super+C) shows what the VM holds on this desktop.
- Containers (2026-10-18): rootless Podman 5.7 and `podman-compose` (no daemon; no recommends), for the projects of ~/Zeke_projects/ia_general_workflow_improvement (the code index `appia` image builds and runs). Cargo/rustc 1.99 come from rustup in ~/.cargo (not on PATH by default: `export PATH=$HOME/.cargo/bin:$PATH`).
- MCP (2026-10-18): the code index `appia` (formerly `ia01`; repo code-index-mcp, folder 01_appia) is registered as `appia` in Claude Code (user scope) and in `agy`; `~/.local/bin/appia-mcp` starts its Podman container for the folder you are in (read-only, no network, your own uid through `--userns=keep-id`; `bin/check-container` in the repo verifies it; one data volume per project, `appia-<folder>-<hash>`; the old `ia01-*` volumes and the `ia01:dev` image are unused and can be removed). Parses Python, Rust, JS/TS(X), Java, Elixir, Go, Bash and Erlang. Since 2026-10-20, in `~`, `/` and any folder that is not a git repository it starts no container and answers MCP with no tools and an `instructions` text ("appia is not active here, and why"), so Claude knows at once instead of seeing a failed server.

## Desktop
- xmonad: action catalog in xmonad.hs, shortcuts in ~/.xmonad/keys.conf,
  custom actions in ~/.xmonad/actions.conf. `keys-editor` edits them and
  `keybinds` (Super+F1 or Super+/) lists them.
- Polybar: one floating pill bar per monitor (rounded ends, pink border), launched by xmonad (launch-bars.sh). Modules:
  workspaces, window title, Wi-Fi, CPU, memory, battery script, date.
- picom (glx, vsync): rounded windows (14 px), blur and slide animation for the dropdown terminal.
- Dropdown terminal: alacritty (class dropterm) with tmux session `drop`,
  toggled by Super+Ctrl+T; it hides before rofi, Super+Enter and Super+E.
- Shutdown (2026-10-21): `system/etc/systemd/logind.conf.d/zz-inhibit-delay.conf` (installed in /etc/systemd/logind.conf.d) sets `InhibitDelayMaxSec=5`; with the CI VM running, shutdown from the login screen waited 30 s for libvirtd's inhibitor. Active from the next boot; the faster shutdown is not timed yet.
- Lock: i3lock through `lock-screen`, by hand with Super+L only. There is no automatic
  lock any more (xss-lock was removed on 2026-10-07: it locked on every lid close).
  The monitor still turns off after 5 minutes of inactivity (DPMS), without a password.
- Theme: Catppuccin Mocha with a pink accent in GTK, Qt, rofi, dunst, CopyQ,
  alacritty, tmux, Neovim, Polybar; Papirus-Dark icons.
- Apps: Claude Desktop 2.42 (Linux beta, Anthropic's apt repo, installed 2026-10-17 without its recommends except QEMU/OVMF/virtiofsd for the Cowork tab; no gnome-keyring, so it may ask to sign in at each launch; `zeke` is in the `kvm` group (log out and in once for Cowork); not signed in or tried yet), Brave (default browser), Thunar, Atril, Neovim (LazyVim),
  Mousepad (default for text files, Catppuccin Mocha colours), micro
  (terminal editor with normal shortcuts), CopyQ,
  KDE Connect and scrcpy for the phone, simple-scan.

## Web apps
- `webapp` (own repo github.com/zekefarioli-arch/webapps, cloned in
  ~/Zeke_projects/webapps, linked into ~/.local/bin by its install.sh) makes a
  site an app with Brave: `--app` in one shared profile, or `--panel` with its
  own profile and the Claude side panel. `--password-store=basic`, so the
  keyring never asks for a password.
- My apps are the list webapps/.config/webapps/apps.conf (stow package, linked
  as ~/.config/webapps/apps.conf); `webapp sync` writes their launchers in
  ~/.local/share/applications. Apps: YouTube (app), WhatsApp, Google Messages and
  Find a job (panel). Find a job is driven by `findajob` (own repo findajob-login, in
  ~/Zeke_projects, Playwright on Brave): `findajob setup` saves the One Login email and
  password once in ~/.config/findajob/credentials (mode 600), and the SMS code comes from
  the last-otp file that otp-to-clipboard writes; Super+G opens it. Google Calendar is its own script, bin/google-calendar.
- xmonad.hs WebAppCrop pushes Brave's tab strip and toolbar out of sight for
  `WebPanel-*` windows (`webappChromePx`); the Claude extension must be
  installed once per panel profile (`webapp extensions <id>`).

## Calendar
- Click on the bar's date: `mini-calendar` (GTK) with Google Calendar events
  from secret iCal addresses in ~/.config/mini-calendar/ics-url (private,
  mode 600, never in the repo) and an "Ask Claude…" box (headless Claude Code
  with the Google Calendar connector).
- Super+Shift+C: Google Calendar in its own Brave profile (tiled; WebPanel-calendar)
  (~/.local/share/calendar-app) with the Claude extension (Ctrl+E).
  Super+Ctrl+C: that profile's extensions and theme.

## Printing and scanning
- CUPS with two queues; cups-browsed is disabled.
  - Brother_MFC_L2700DN (default): driverless IPP, two-sided.
  - Canon_iX6800: through the CUPS server it is plugged into (USB).
  - Addresses are not in the repo: ~/.config/printers.env (BROTHER, SERVER).
- Scanner: Brother MFC-L2700DN through sane-airscan (WSD), pinned in
  /etc/sane.d/airscan.conf. Setup: scripts/setup-printers.sh.

## Claude Code
- `machine` (bin/.local/bin/machine, linked in ~/.local/bin): name label for `claude-new` (now `zeke-ryzen9-desktop`), and `machine ip N` to choose a fixed address; its private list of taken numbers is
  home-infra/config/machine-network.conf, linked as ~/.config/machine/network.conf. The router reservation (`.23`) is made.
- The session tools live in their own repo, github.com/zekefarioli-arch/claude-tools
  (cloned in ~/Zeke_projects/claude-tools, linked into ~/.local/bin by its
  install.sh): `claude-new` (sessions named <path>_NNN@<machine>, path = the
  folder from ~ with dashes, machine = first 4 of /etc/machine-id or the text of
  ~/.config/claude-new/machine), `claude-pick` (rofi launcher on Super+a: folders,
  sessions, context usage, handoff from 50%, git marks) and `claude-fresh`
  (updates the project from GitHub before a session when it is only behind and
  clean). How to use them: docs/SESSIONS.md here, and the repo's README.
- `pc-admin` (repo ~/Zeke_projects/pc-admin, linked in ~/.local/bin, 2026-10-29): numbered session for fixing the computer, with dotfiles and home-infra added (`--add-dir`); tested with a fake claude, not yet in a real session.
- `claude/.config/claude-tools/repos` lists the extra repos `claude-fresh`
  updates when Claude is opened in ~ (~/dotfiles and claude-tools are implicit).
- ~/CLAUDE.md (dotfiles package `claude`) holds the standing instructions and
  imports ~/.claude/STATE.local.md, a per-machine symlink to this file (the
  Ryzen 9 desktop) or to another machine's own state file; the link is not in the repo.
  Claude's own memory is in ~/.claude/projects/-home-zeke/memory/.

## In progress
Making Claude Code sessions easy to launch and share between the Ryzen 9 desktop and the Arch desktop, one
step at a time (I decide each step). Steps 1 to 7 are done; the open ideas (8a to 8f: Arch dotfiles up to
date, sharing sessions, ssh hardening, tmux sessions, ephemeral CI VMs) and the full list are in
[ROADMAP.md](ROADMAP.md), which holds the plan; read it when the work touches them.

## Pending
The full list is in [PENDING.md](PENDING.md); read it before proposing new work. The ones that matter most:
- ssh passwords are still on and /etc/sudoers.d/zeke-nopasswd is still there (also a stray one on the Arch desktop).
- The backup on hs-gateway (~/migration-2026-10-16, holds private keys) is still to delete.
- CI VM trust: user `ci` is root inside the VM and its network reaches the LAN; fine for my own repos only.
- Hyprland (user `zeke-hypr`, and a config in ~/.config/hypr): not tried on screen yet.
- Firmware: BIOS 1.54 and Secure Boot updates (`sudo fwupdmgr update`, charger plugged in).

## Known issues
- CopyQ's tray icon can be missing after a login if CopyQ starts before Polybar's tray; `copyq exit; copyq --start-server &` brings it back.
- picom died once without a trace (no log, no core dump) and was started again
  by hand; if blur or the dropdown animation stop working, check
  `pgrep -x picom` and start it with `picom &` (autostart.sh only does it at login).
- A web app window opened before an `xmonad --restart` can stay unmanaged (no
  workspace, missing from Super+Tab); close it and open it again from rofi.
- The dropdown terminal does not hide by itself when it loses the focus
  (nsHideOnFocusLoss has no effect); it hides on the shortcuts listed above.
- The local DNS (router) fails to resolve some hosts, such as
  download.brother.com; resolve through another DNS if a download fails.
