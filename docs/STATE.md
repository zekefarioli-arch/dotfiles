# System state

The current state of the laptop: what is installed and configured, what is
pending and what is known to be broken. Claude Code reads this at the start of
every session; keep it short and true, and update it in the same commit as
the change it describes. History goes in CHANGELOG.md, reasons in
DECISIONS.md.

_Last updated: 2026-10-07_

## Machine
- Lenovo ThinkPad X13 Gen 1 AMD (20UGS2Q500), 30 GiB RAM, 236 GB NVMe.
- BIOS R1CET79W (1.48).
- Fedora 44, kernel 7.2.x, btrfs root with snapper snapshots.
- Wi-Fi only; timezone Europe/London, set
  automatically by a NetworkManager dispatcher script.
- User `zeke`; LightDM starts the xmonad session.

## Desktop
- xmonad: action catalog in xmonad.hs, shortcuts in ~/.xmonad/keys.conf,
  custom actions in ~/.xmonad/actions.conf. `keys-editor` edits them and
  `keybinds` (Super+F1 or Super+/) lists them.
- Polybar: one bar per monitor, launched by xmonad (launch-bars.sh). Modules:
  workspaces, window title, Wi-Fi, CPU, memory, battery script, date.
- picom (glx, vsync): blur and slide animation for the dropdown terminal.
- Dropdown terminal: alacritty (class dropterm) with tmux session `drop`,
  toggled by Super+Ctrl+T; it hides before rofi, Super+Enter and Super+E.
- Lock: i3lock through `lock-screen`, by hand with Super+L only. There is no automatic
  lock any more (xss-lock was removed on 2026-10-07: it locked on every lid close).
  The monitor still turns off after 5 minutes of inactivity (DPMS), without a password.
- Theme: Catppuccin Mocha with a pink accent in GTK, Qt, rofi, dunst, CopyQ,
  alacritty, tmux, Neovim, Polybar; Papirus-Dark icons.
- Apps: Brave (default browser), Thunar, Atril, Neovim (LazyVim),
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
  ~/.local/share/applications. Apps: YouTube (app), WhatsApp and Google Messages
  (panel). Google Calendar is its own script, bin/google-calendar.
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
  laptop) or to the desktop's own state file; the link is not in the repo.
  Claude's own memory is in ~/.claude/projects/-home-zeke/memory/.

## In progress
Making Claude Code sessions easier to launch and to share between this laptop
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
7. NEXT, ideas to design (not built; give options for one step at a time):
   a) Git repos on the LAN: bare repos on one of the home servers reached
      over ssh as the central copy, with tests still run on the laptop and the Arch
      desktop, and GitHub or GitLab as an off-site mirror. Open: which server, whether a
      web UI (Gitea or Forgejo) is worth it, and what claude-fresh then fetches from.
   b) Bring the Arch dotfiles up to date: they are 85 commits behind with 3 uncommitted
      files that conflict (xmonad.hs, autostart.sh, launch-polybar.sh), and its xmonad.hs is
      an older, simpler design. Either merge keeping the local edits, or split the
      configuration per machine (the real fix for dotfiles mixing both computers). Super+a
      on Arch is one line in its own xmonad.hs, compiled, and still needs `xmonad
      --restart` there to load it.
   c) Sharing Claude sessions between the machines (A: do not share, recommended for now;
      B: private repo with a sync script; C: Syncthing; D: rsync over ssh). The names
      already work across machines.
   d) Harden ssh on the laptop and the Arch desktop: keys first, then passwords off and a
      firewall rule for the LAN only (the details are in home-infra).
   e) Open claude-pick sessions inside tmux so the phone can attach to them.
8. LATER, a private repo for Claude's memory and settings (memory/,
   settings.json, keybindings.json, skills), not decided.

## Pending
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

- Network: this laptop has a fixed address through a DHCP reservation in the home router.
  The addresses, the machines and how they connect (router, media server, Arch desktop,
  printers, Docker services) are documented in the private repo home-infra, a private
  GitLab project cloned in ~/Zeke_projects/home-infra, and not here, because this repo is
  public. Private repos could not be created on GitHub on 2026-10-07 (HTTP 500), so that one
  lives on GitLab; the public repos (dotfiles, claude-tools, webapps) stay on GitHub. GitLab
  is reached over ssh with a key that ~/.ssh/config offers for gitlab.com only.
- Arch desktop: ssh access from this laptop is set up and claude-tools and webapps are
  installed there; Super+a there is one line in its own xmonad.hs, compiled, and needs
  `xmonad --restart` on that machine to load. Details in home-infra.
- ssh: hardening is planned for this laptop and the Arch desktop (keys first, then passwords
  off and a firewall rule for the LAN only). The current state and the plan are in home-infra.

## Known issues
- picom died once without a trace (no log, no core dump) and was started again
  by hand; if blur or the dropdown animation stop working, check
  `pgrep -x picom` and start it with `picom &` (autostart.sh only does it at login).
- A web app window opened before an `xmonad --restart` can stay unmanaged (no
  workspace, missing from Super+Tab); close it and open it again from rofi.
- The dropdown terminal does not hide by itself when it loses the focus
  (nsHideOnFocusLoss has no effect); it hides on the shortcuts listed above.
- The local DNS (router) fails to resolve some hosts, such as
  download.brother.com; resolve through another DNS if a download fails.
