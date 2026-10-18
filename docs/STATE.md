# System state

The current state of this machine (the Ryzen 9 desktop): what is installed and configured, what is
pending and what is known to be broken. Claude Code reads this at the start of
every session; keep it short and true, and update it in the same commit as
the change it describes. History goes in CHANGELOG.md, reasons in
DECISIONS.md.

_Last updated: 2026-10-18_

## Machine
- Ryzen 9 desktop (the 1 TB NVMe, Crucial P310, moved here from the ThinkPad X13 on 2026-10-17; it was the laptop's disk since
  2026-10-13). Hostname `zeke-ryzen9-desktop`. The ThinkPad no longer has this system. 14 GiB RAM now, a 32 GiB upgrade is
  planned. KVM works (AMD-V, `kvm_amd` loaded). Other hardware details: not written down yet.
- Ubuntu 26.04 LTS (server install, minimized, plus the desktop of these dotfiles), kernel 7.0.x, ext4 on LVM (the root volume is 300 GB of the 931 GB disk, extended from the installer's 100 GB on 2026-10-18; about 628 GB stay unassigned in the volume group `ubuntu-vg` for later: `sudo lvextend -r -L +NG /dev/ubuntu-vg/ubuntu-lv`)
  (replaced Fedora 44 on 2026-10-17; plan and package map in docs/UBUNTU-MIGRATION.md; the old Fedora
  disk is kept in a USB enclosure). Hostname `zeke-ryzen9-desktop` (was `zeke-thinkpad`); ufw on (ssh from the LAN only).
- Wired (`eno1`, MAC 30:c5:99:d2:3b:ee), managed by NetworkManager through netplan; timezone Europe/London, set
  automatically by a NetworkManager dispatcher script. Fixed address `.23` (a DHCP reservation by this MAC in the router, outside
  the pool; made and checked on 2026-10-17). The Wi-Fi card (`wlp8s0`) is down;
  the netplan file for `CASITA_5G` is left over from the laptop (it matches `wlp3s0`) and does nothing here.
- User `zeke`; LightDM starts the xmonad session.
- Virtualization (2026-10-17): KVM and libvirt (`qemu:///system`, packages without recommends; user `zeke` is in `libvirt` and `kvm`).
  One VM, `ci-runner`: Ubuntu Server 26.04.1, 6 vCPU, 6 GB, 80 GB, UEFI, CPU shares 256, autostart. It hosts a persistent GitHub Actions
  runners for the private repos `mock-oclock` and `due-card` (one runner each, label `ryzen-vm`; the repository variable `CI_RUNS_ON` of each
  switches its CI between the VM and GitHub's runners; a lock makes the two repos share the VM one job at a time). Two password-protected web panels for it (the runner's current step, the VM's load) are served by nginx on this
  desktop and reachable from the home network only (ufw). The VM may use 6 GB of the 14 GiB of RAM and gives back what it frees (free page reporting on its memory balloon, 2026-10-17): idle it holds about 1.6 GiB of the host, not 5.7; with the 32 GB planned it is comfortable. Runbook and addresses: home-infra
  `runbooks/ci-runner.md` (private repo). The suspend setting must stay off while it serves CI.
- Containers (2026-10-18): rootless Podman 5.7 and `podman-compose` (no daemon; no recommends), for the projects of ~/Zeke_projects/ia_general_workflow_improvement (the code index `ia01` image builds and runs). Cargo/rustc 1.99 come from rustup in ~/.cargo (not on PATH by default: `export PATH=$HOME/.cargo/bin:$PATH`).
- MCP (2026-10-18): the code index `ia01` (repo code-index-mcp) is registered as `ia01` in Claude Code (user scope) and in `agy`; `~/.local/bin/ia01-mcp` starts its Podman container for the folder you are in (read-only, no network, your own uid through `--userns=keep-id`; `bin/check-container` in the repo verifies it; one data volume per project, `ia01-<folder>-<hash>`, the old shared `ia01_data` is gone). Parses Python, Rust, JS/TS(X), Java, Elixir, Go, Bash and Erlang.

## Desktop
- xmonad: action catalog in xmonad.hs, shortcuts in ~/.xmonad/keys.conf,
  custom actions in ~/.xmonad/actions.conf. `keys-editor` edits them and
  `keybinds` (Super+F1 or Super+/) lists them.
- Polybar: one floating pill bar per monitor (rounded ends, pink border), launched by xmonad (launch-bars.sh). Modules:
  workspaces, window title, Wi-Fi, CPU, memory, battery script, date.
- picom (glx, vsync): rounded windows (14 px), blur and slide animation for the dropdown terminal.
- Dropdown terminal: alacritty (class dropterm) with tmux session `drop`,
  toggled by Super+Ctrl+T; it hides before rofi, Super+Enter and Super+E.
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
- `claude/.config/claude-tools/repos` lists the extra repos `claude-fresh`
  updates when Claude is opened in ~ (~/dotfiles and claude-tools are implicit).
- ~/CLAUDE.md (dotfiles package `claude`) holds the standing instructions and
  imports ~/.claude/STATE.local.md, a per-machine symlink to this file (the
  Ryzen 9 desktop) or to another machine's own state file; the link is not in the repo.
  Claude's own memory is in ~/.claude/projects/-home-zeke/memory/.

## In progress
Making Claude Code sessions easier to launch and to share between the Ryzen 9 desktop
and the Arch desktop, one step at a time (I decide each step before it is built):
1. DONE: `claude-pick`, a rofi launcher on Super+a (folder, then session) showing
   the context usage of each folder's last session and suggesting a new session
   with a handoff from 50% (green < 35%, yellow < 50%, red from 50%; window
   200000 tokens). Decided: rofi rather than fzf, ~ is the default row, folders =
   already worked in + ~/Zeke_projects + a tree browser. The key, rofi and
   Alacritty were tried by launching a handoff session from it.
2. DONE, freshness: `claude-fresh` (used by claude-new and by claude-pick when
   resuming) runs a 4 s `git fetch` and, if the project is only behind and clean,
   `git pull --ff-only`; dirty, diverged or ahead it only warns (and waits for
   Enter); never merges, forces or pushes; offline is skipped. In ~ it works on
   ~/dotfiles, claude-tools and the repos in claude/.config/claude-tools/repos.
   claude-pick marks folders with ↑ not pushed, ✎ uncommitted, ↓ behind. Not
   tried yet: the update itself between the two computers.
3. DONE 2026-10-07, the split for the Arch desktop (dotfiles mixes Fedora-only
   things: scripts/, system/, STATE.md, laptop scripts in bin/): two public
   repos, `claude-tools` (claude-new, claude-pick, claude-fresh, their tests, an
   install.sh for pacman, dnf and apt) and `webapps` (`webapp`, with the list of
   apps in apps.conf, launchers generated by `webapp sync`, the Brave command
   detected, the xmonad crop as a snippet). Both are documented in the
   `super-zeke` voice. On Arch: clone both, run their install.sh, stow
   `webapps` and `claude` from dotfiles, `webapp sync`, `xmonad --recompile`.
   Done on Arch on 2026-10-07: both repos cloned in ~/Projects and installed, with
   ~/.config/claude-tools/{projects_dir,repos}, machine name `arch`, terminal wezterm
   (no alacritty there; claude-pick detects it). Super+a on Arch is one line added to
   its own xmonad.hs (additionalKeysP) and compiled; it needs `xmonad --restart` there.
   Arch's dotfiles are 85 commits behind with 3 uncommitted files that conflict, so no
   pull yet: merging them (or a per-machine split) is still to do.
4. DONE 2026-10-07, session names: `<path>_NNN@<machine>`, for example
   `Zeke_projects-mini-calendar_002@arch` (`home_001@d36e` in ~). The machine
   only labels: the last session of a folder is the most recent one of any
   machine and the next number is the highest plus one, so the sessions of two
   computers in the same place do not collide. Old names (`d36e_home_001`,
   <machine>_<folder>_NNN of this machine) still count; fedora_desktop003 and
   unnamed ones do not. Chosen: B with the full path; readable machine names
   are optional (`./install.sh --name arch`). Not tried yet: starting a real
   session with the new name (checked with a fake claude and my real sessions).
5. DONE 2026-10-07, `claudio`: the console-only mode of claude-pick (numbered menus in the
   terminal you are in, the session starts there; `claude-pick --console`), and
   claude-pick falls back to it by itself with no display or no rofi. For a TTY or ssh
   from the phone. Installed on the laptop and on Arch.
6. DONE 2026-10-07, claude-tools also: the terminal is detected or set (CLAUDE_TERMINAL,
   ~/.config/claude-tools/terminal; wezterm on Arch), the projects folder can be a file
   (~/.config/claude-tools/projects_dir; ~/Projects on Arch), and claude-pick puts
   ~/.local/bin first in PATH (a key bound in xmonad has a minimal one).
7. DONE 2026-10-07, repos on the home server: the central copy of my repos is a git hub on
   the media server (bare repos; a restricted `git` user that can only run git; each key
   marked `restrict`), reached over ssh with the alias `hub`. Each clone has `origin` on the
   hub with two push URLs (the hub and GitHub, or GitLab for the private repo) and a spare
   remote (`github`, `gitlab`) for when I am away from home; the tests still run on the
   laptop and the Arch desktop. Wired: dotfiles, claude-tools, webapps, home-infra, and a
   repo of the Arch desktop that had no remote. Not wired yet: my other repos and the Arch
   checkout of dotfiles. The addresses and details are in the private repo home-infra.
   Wired later the same day: all my other own repos (17 in the hub), on the laptop, the
   Arch desktop and the router. Three of them (mined, job-tracker, kanban-app) have two
   commits each that are on the hub but not yet on GitHub; a `git push` in each clone
   sends them to both.
8. NEXT, ideas to design (not built; give options for one step at a time):
   a) DONE: the commits made on 2026-10-07 in six clones, and a repo that had two commits
      only on the hub, are pushed to the hub and to GitHub; the hub and GitHub agree. One
      experiment branch was published only after taking an address of the home network
      out of its script (it reads it from an environment variable now). The few log and backup
      files that were left out are now ignored by a .gitignore in each repo (pushed).
      Nothing is pending in the wired clones. (The hub and the two home servers now have nightly restic backups, crossed
      between the servers, set up and verified on 2026-10-07 with a restore drill; and
      mock-oclock/private has a third copy in a private GitLab project. Details and the
      commands to check them are in the private repo home-infra. An alert
      now e-mails me when a night fails (each job pings healthchecks.io, set up on 2026-10-07);
      still open, postponed for budget: an encrypted off-site copy.)
   b) Bring the Arch dotfiles up to date: they are 85 commits behind with 3 uncommitted
      files that conflict (xmonad.hs, autostart.sh, launch-polybar.sh), and its xmonad.hs is
      an older, simpler design. Either merge keeping the local edits, or split the
      configuration per machine (the real fix for dotfiles mixing both computers). Super+a
      on Arch is one line in its own xmonad.hs, compiled, and still needs `xmonad
      --restart` there to load it.
   c) Sharing Claude sessions between the machines (A: do not share, recommended for now;
      B: private repo with a sync script; C: Syncthing; D: rsync over ssh). The names
      already work across machines.
   d) Harden ssh on this desktop and the Arch desktop: keys first, then passwords off and a
      firewall rule for the LAN only (the details are in home-infra).
   e) Open claude-pick sessions inside tmux so the phone can attach to them.
9. LATER, a private repo for Claude's memory and settings (memory/,
   settings.json, keybindings.json, skills), not decided.

## Pending
- CI runner VM (2026-10-17): `mock-oclock` (pull request 82, ADR 046 and 047) and `due-card` (ADR 0028 in that repo) run their CI on it
  while `CI_RUNS_ON` is set. The panels are plain HTTP on the home network: not tried over the VPN. Not tried yet: a second runner per repo so
  two jobs overlap (the lock allows one at a time in the VM), and an ephemeral runner. Not tried yet: a second runner so two jobs overlap, and an ephemeral runner (needs a host-side
  controller so no GitHub token lives in the VM). Suspend: systemd's suspend.target is not masked, so it is not proven off; check before relying on the VM overnight.
- Hyprland (2026-10-17): installed system-wide; the trial is the user `zeke-hypr` (admin; pick "Hyprland" at the login screen),
  config in its `hyprland` branch (step 1: Catppuccin, xmonad shortcuts, waybar). Not tried on screen yet. Ported next, if it is kept:
  keybinds/keys-editor, dropdown terminal, web-app panels (WebAppCrop is xmonad-only), the six scripts that use xdotool.
- Brightness keys need the `video` group: log out and in once (2026-10-17) so the session has it.
- Ubuntu migration, still open (2026-10-17): ssh passwords are still on and
  /etc/sudoers.d/zeke-nopasswd (setup convenience) is still there; turn passwords off
  (`/etc/ssh/sshd_config.d/10-no-password.conf`) and remove the sudoers file when the setup is accepted.
  Antigravity IDE 1.107 (tarball 2.5.5) and the CLI `agy` 1.4.0 are installed again (2026-10-17); the IDE still needs its AppArmor profile
  (`system/etc/apparmor.d/antigravity-ide`, installed in /etc/apparmor.d on 2026-10-17, steps in its header) or Ubuntu blocks its sandbox.
  Both are signed in; `agy --print "/usage" --output-format json --mode plan < /dev/null` shows the quota left.
  Not tried on screen yet: the look of the bars and picom's rules, Super+Ctrl+T, `findajob` with Alt+G,
  Super+O, sound, brightness and the lid. Keyboard layout is `gb`; say if it should be another.
  The backup on hs-gateway (~/migration-2026-10-16, holds private keys) is still to delete when you are sure.
  Delete the stray /etc/sudoers.d/zeke-nopasswd. on the Arch desktop (created by mistake).
- Ubuntu migration done on 2026-10-17 (plan in docs/UBUNTU-MIGRATION.md); the old 236 GB Fedora disk is kept as the way back. The picom 12.5 check is still to do.
- Login screen (2026-10-17): LightDM greeter with a Catppuccin wallpaper (/usr/share/backgrounds/login-kitty.jpg) and the theme and cursor copied to /usr/share; not seen on screen yet.
- Firmware: BIOS 1.54 and Secure Boot KEK/UEFI CA 2023 plus dbx updates are
  available. Needs the charger plugged in: `sudo fwupdmgr update`.
- Passwordless sudo (/etc/sudoers.d/zeke-nopasswd) was added for the setup;
  remove it when the setup is done.
- Claude sessions: the older fedora_desktop003 is not matched by `claude-new`;
  open it with `claude -r`.
- Claude sessions: no way yet to delete old ones (to do later).
- Web apps: FireShot (whole page to PDF or image) is not installed in the shared
  profile yet: `webapp extensions`, then set its shortcut in
  brave://extensions/shortcuts (suggested Alt+Shift+P for PDF, Alt+Shift+I for
  image; xmonad has no Alt shortcuts).
- Menus: the picom catch-all rule (no blur on translucent windows) and the flat
  GTK menus (gtk.css) are not confirmed on screen; open Brave's menu and
  Mousepad's File menu to check them.
- Web apps: Google Messages needs its Claude extension installed
  (`webapp extensions google-messages`) and the phone pairing again. A launcher
  "WhatsApp Web" (brave-hnpfjng...) may reappear in rofi; remove it from
  brave://apps in the WhatsApp window.
- claude-fresh: needs a real test between the two computers (push from one, open
  the project on the other). Nothing is pulled when the tree has uncommitted
  changes, by design.
- claude-pick: not tried yet: "Other folder…", resuming an old session, a
  folder with no sessions.
- Arch desktop: clone dotfiles, claude-tools and webapps (in ~/Zeke_projects),
  run each install.sh (`./install.sh --name arch` in claude-tools, `--example`
  not needed because the list comes from dotfiles), then `stow --no-folding -t ~
  webapps claude xmonad` plus the common packages, `webapp sync`, `xmonad
  --recompile`, its own docs/STATE-arch.md linked as ~/.claude/STATE.local.md,
  Brave (AUR brave-bin) and ~/.config/printers.env if it prints;
  setup-printers.sh uses dnf, so it needs a pacman version there.
- Calendar: creating and editing events from the mini calendar itself
  (OAuth) is deferred; for now the Ask Claude box does it.
- Polybar hot-plugging was only tested with simulated monitors; confirm with
  a real external monitor.

- Network info: Super+I (`show-ip`, runs `net-info`) shows a notification with the connection, the IP, the gateway and the name of this machine; a click on the bar's Wi-Fi module does the same. Added and tried on screen on 2026-10-17.
- VPN to the house: NetworkManager connection `wg-home` (WireGuard, split tunnel, no autoconnect), toggled with Super+O (`home-vpn`). Needed away from home: a phone hotspot does not carry the phone's own VPN to tethered devices. The client key lives only in NetworkManager; the server name and subnets are in home-infra.
- Network: this desktop has a fixed address (`.23`) through a DHCP reservation in the home router.
  The addresses, the machines and how they connect (router, media server, Arch desktop,
  printers, Docker services) are documented in the private repo home-infra, a private
  GitLab project cloned in ~/Zeke_projects/home_network/home-infra, and not here, because this repo is
  public. Private repos could not be created on GitHub on 2026-10-07 (HTTP 500), so that one
  lives on GitLab; the public repos (dotfiles, claude-tools, webapps) stay on GitHub. GitLab
  is reached over ssh with a key that ~/.ssh/config offers for gitlab.com only.
- Arch desktop (2026-10-08): fully updated and on the shared configuration: dotfiles on `main` (the old edits are kept in branch `arch-local`), stowed with `--no-folding`: xmonad polybar bin alacritty tmux rofi dunst gtk qt x11 xdg micro picom starship. Not stowed there: nvim, bash (its own .bashrc), claude, webapps, copyq. Picom is off there (`~/.config/picom/disabled`). alacritty, micro, scrcpy, android-tools and Papirus installed; SDDM with the Catppuccin pink theme on one monitor (`system/arch/sddm-setup.sh`); hot-plug of monitors works through the shared xmonad.hs. Still to do there: its own STATE file (step 5) and trying everything on screen.
- Arch desktop: ssh access from this desktop is set up and claude-tools and webapps are
  installed there; Super+a there is one line in its own xmonad.hs, compiled, and needs
  `xmonad --restart` on that machine to load. Details in home-infra.
- ssh: hardening is planned for this desktop and the Arch desktop (keys first, then passwords
  off and a firewall rule for the LAN only). The current state and the plan are in home-infra.

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
