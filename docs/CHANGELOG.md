# Changelog

What changed on the system, newest first, one line per change that matters
to me. Commit messages have the details (`git log`); this file is the summary
I can read in a minute. Add a line in the same commit as the change.

## 2026-10-16
- Rule for the router (hs-gateway) loosened at my request: Claude may write files in my home directory there when I ask for it (the migration backup); services, CI, containers and its firewall or Docker setup stay off limits. DECISIONS.md has the entry.
- Planning the move of the laptop from Fedora 44 to Ubuntu Server 26.04 LTS: docs/UBUNTU-MIGRATION.md has the install steps, the Fedora-to-Ubuntu package map (checked against the Ubuntu index), what to restore and the risks, and scripts/ubuntu-post-install.sh installs it all (checked, not yet run). `findajob-login` is now a private GitLab project and a backup of /home and of the system files is on the gateway.

## 2026-10-17
- New private repo `fullstack-blueprint` (decisions extracted from mock-oclock and due-card, backend selection with Java and Elixir first, stacks, generator, skill) and a model routing for it and for frontend-kit: seven agents (`fb-*`, `fk-*`) that carry their own model (haiku for mechanical work, sonnet for building, opus for architecture and design), changed in one `models.conf` per kit. Installed on the ThinkPad with each kit's `install.sh` (links into `~/.local/bin`, `~/.claude/skills` and `~/.claude/agents`).
- The CI runner VM also serves the private repo `due-card` (a second runner; its `ci.yml` reads `CI_RUNS_ON` like `mock-oclock`'s, ADR 0028 there). The two runners share a lock so one job runs at a time in the VM. Details in home-infra (`runbooks/ci-runner.md`).
- Fix: xmonad said "unknown action: show-ip" after the Super+I commit, because actions.conf named the action `custom-show-the-network-ip-address-and-name-of-` (an auto-generated name) while keys.conf used `show-ip`. The action is now called `show-ip`, like the other custom actions.
- CI runner for the private repo `mock-oclock` in a KVM virtual machine on the Ryzen 9 desktop (Ubuntu Server 26.04, 6 vCPU, 6 GB), because the free GitHub Actions minutes ran out. The repository variable `CI_RUNS_ON` (pull request 82 of that repo) switches the CI jobs between it and GitHub's runners; all five passed on it. Two always-on, password-protected web panels on the Ryzen show the runner's current step and the VM's load (nginx proxy, ufw open to the home network only). Details, with addresses, in home-infra (`runbooks/ci-runner.md`).
- Alacritty font size is now per machine: `size` left alacritty.toml, which imports ~/.config/alacritty/local.toml (not in the repo; 11.0 on the ThinkPad, 8.5 on the Ryzen 9). The shared 8.5 made the ThinkPad's letters too small after a pull.
- New docs/STATE-thinkpad.md for the ThinkPad (Fedora 44 on its 256 GB disk, hostname `zeke-thinkpad-x13`, address changed in the router), because STATE.md now describes the Ryzen 9 desktop; ~/.claude/STATE.local.md on the laptop points to it, and ~/CLAUDE.md says which file belongs to which machine.
- Super+I (`show-ip`) shows the home-network IP and the machine's name in a notification: `net-info` now ends with a `Host` line.
- The 1 TB disk now runs in the Ryzen 9 desktop: hostname and `claude-new` label are `zeke-ryzen9-desktop` (were `zeke-thinkpad` / `temp-zeke-thinkpad`). Fixed address `.23`: reservation added in the router by hand and the desktop renewed onto it (it was `.140` by DHCP, inside the pool).
- Login screen: Catppuccin wallpaper in the LightDM greeter (login box moved up to 33%), and the post-install script copies the theme and cursor to /usr/share because the greeter cannot read ~. Neovim plugins updated (lazy-lock.json).
- Hyprland 0.53.3 installed next to xmonad (waybar, hyprlock, hyprpaper, grim, slurp, wl-clipboard, cliphist, xdg-desktop-portal-hyprland) to try it in a separate admin user `zeke-hypr`; its config is a stow package `hypr` on the branch `hyprland` of that user's clone of dotfiles. `zeke` and xmonad are untouched.
- CopyQ now stores screenshots and other images: Ubuntu ships the image plugin in `copyq-plugins` (added to ubuntu-post-install.sh). Its tray icon also showed up in Polybar after restarting CopyQ (it had missed the bar, probably because the bar was relaunched after CopyQ started); it may be missing again after a login if CopyQ starts before the bar.
- Antigravity is back: IDE from Google's tarball in ~/.local/opt/antigravity-ide and the CLI `agy` from Google's installer (checksum verified; I removed the PATH lines it added to .bashrc, .bash_profile and .profile). The IDE needs an AppArmor profile to start (system/etc/apparmor.d/antigravity-ide).
- New `machine` tool: `machine status`, `machine name NAME [--hostname]` and `machine ip N` (picks the last byte of a fixed address, refuses numbers taken by the house or inside the DHCP pool, checks it is free and prints the router command; it never writes to the router). This laptop's session label is now `temp-zeke-thinkpad` because its disk will move to the new desktop.
- Terminal font size 11 -> 8.5 (the laptop screen is 166 ppp at 96 dpi, so 11 looked huge).
- Ubuntu: the brightness keys needed a udev rule (system/etc/udev/rules.d/90-brightnessctl.rules) and the `video` group; log out and in once to use them.
- The bar vanished after a login once; launch-bars.sh now logs polybar's output to $XDG_RUNTIME_DIR/polybar.log so the cause shows if it happens again.
- The laptop runs Ubuntu 26.04 LTS instead of Fedora 44: packages installed with scripts/ubuntu-post-install.sh, home and settings restored from the backup on the gateway, NetworkManager manages Wi-Fi, printers, scanner, `wg-home` VPN, `findajob`, xmonad (0.18.0), polybar and picom checked. See DECISIONS.md.

## 2026-10-14
- Decision recorded (nothing installed): the CI runner will live in a Debian VM on the new Ryzen desktop, whose host stays Fedora for work. See DECISIONS.md.
- Decision recorded (nothing installed): pipelines for private repos spend fewer GitHub minutes first, and any self-hosted runner goes on a dedicated machine in an ephemeral, limited container, never on the router. See DECISIONS.md.

## 2026-10-13
- Find a job (jobs.service.gov.uk, GOV.UK One Login): `findajob` (repo findajob-login) signs in with a saved password and types the SMS code by itself; Super+G opens it, and it is the `find-a-job` panel app (Claude side panel). otp-to-clipboard also writes the last code and its time to $XDG_RUNTIME_DIR/last-otp, and dunst runs it for SMS shown by the Google Messages web app as well as by KDE Connect.
- Laptop disk replaced by a 1 TB NVMe. The old layout was copied, then the btrfs partition (p3) was grown to the end of the disk with sfdisk and `btrfs filesystem resize max /`, online: 929 GB, 912 GB free, no disk errors.

## 2026-10-12
- Arch desktop: Gemini from Claude Code sessions through the Antigravity CLI `agy` (Google AI Pro plan, headless), with the Gemini CLI and an AI Studio API key as fallback; the user skill `gemini` in the `claude` package explains both (second reader, blind solves, second opinions).

## 2026-10-10
- Super+O toggles the WireGuard VPN to the house (`home-vpn`, NetworkManager connection `wg-home`, split tunnel: only the home subnets) with a notification. Tested from the phone's hotspot: the home servers answer, and git pushes to the hub.
- Bar window title: known apps (Brave, web apps, terminals, code, Thunar) show only the window title without the class or Brave's " - Brave" suffix; terminals and web apps got their own icons. Other windows keep "class - title".
- Bar window title: web apps (WebPanel-*) show only the page title, without the internal class name, and titles are cut at 32 characters instead of 45, because a long title (Google Calendar) ran into the right-hand modules of the pill-shaped bar.
- Battery module: the time next to the percentage now has a plug icon instead of a second battery icon (red crossed-out plug while discharging, green plug while charging), because the old end-of-line battery looked like the level icon.
- Rounder look: Polybar is a floating pill (margins, fully round ends, pink border all round), picom rounds windows to 14 px (not the bar, fullscreen or the screenshot selector), rofi and dunst radii raised to 18-22 px. Polybar cannot round single modules, so there are no per-module pills.

## 2026-10-08
- Polybar: the bar's screen number now reaches xmonad-log.sh through the environment, because polybar 3.7.2 on Arch did not expand ${env:SCREEN:0} in `exec`. This is the real cause of the "current monitor" icon lighting up on both bars.
- Polybar: launch-bars.sh falls back to RandR when Xinerama is inactive (Arch desktop, legacy NVIDIA driver). Before, both bars read screen 0 and the "current monitor" icon lit up or went out on both at once.
- Arch desktop has its own state file, docs/STATE-arch.md, linked as ~/.claude/STATE.local.md there, and ~/CLAUDE.md now links to the shared one, so Claude sessions on Arch know my rules and the machine.
- Polybar battery module: battery-status prints nothing when there is no battery, so the shared bar shows no battery on the Arch desktop.
- Dropdown terminal: slower slide animation (0.35 s to show, 0.45 s to hide, was 0.2 and 0.3), because it looked rough on the Arch desktop.
- Arch desktop now uses the same dotfiles as the laptop (main, stowed with --no-folding): xmonad, polybar, rofi, dunst, GTK/Qt themes, alacritty, tmux, starship. Backups of what was replaced are in ~/.config-backup-2026-10-08 on that machine.
- autostart.sh is shared with the Arch desktop: a machine can skip picom by creating ~/.config/picom/disabled (a local file, not in the repo).
- Arch desktop: Catppuccin Mocha pink login (SDDM) shown on one monitor only, alacritty, micro, scrcpy, android-tools and Papirus installed, ble.sh removed, same Starship prompt and alacritty/tmux files as the laptop, and xmonad there re-arranges the monitors and the bars when one is plugged or unplugged. The Arch-only root script is `system/arch/sddm-setup.sh`; the xmonad changes are on the `arch-local` branch until Arch gets the shared configuration.

## 2026-10-07
- Postponed the encrypted off-site backup (no budget now); the options are in the private repo home-infra.
- Alert for failed nightly backups: each job pings healthchecks.io when it succeeds and the service e-mails me if a ping does not arrive. The details (keys, hosts) are in the private repo home-infra.
- A .gitignore line in each of three repos for the log, browser-session and backup files that were left out of commits (committed and pushed to the hub and to GitHub).
- Committed the uncommitted work of six clones (7 commits) and pushed everything to the hub and to GitHub; one experiment branch went to GitHub only after I took an address of the home network out of its script. A few log and backup files were left out on purpose.
- Nightly backups of the two home servers and of the git hub (restic, crossed between the servers, a restore drill passed). A private repo of the Arch desktop that had no remote now has a third copy in a private GitLab project. Details in home-infra (private).
- All my other own repos are wired to the git hub too (17 in total, on the laptop, the Arch desktop and the router). Three have two commits each that are on the hub but not yet on GitHub.
- Git hub on the home media server: the central copy of my repos, reached over ssh with a restricted `git` user. dotfiles, claude-tools, webapps and home-infra now have `origin` on the hub, with a second push URL on GitHub or GitLab, so one `git push` updates both. Details in home-infra (private).
- `claudio` (claude-tools): claude-pick without a screen, as numbered menus in the terminal; claude-pick falls back to it with no display or no rofi.
- Removed the automatic screen lock (`xss-lock`): it locked on every lid close. Super+L still locks by hand.
- This laptop has a fixed IP (a DHCP reservation in the home router). A dedicated ssh key (not in
  the repo) is authorised on the other machines of the house; ssh-askpass-rofi lets ssh ask for a
  password through rofi when there is no terminal. The network is documented in home-infra (private).
- Session names are now `<path>_NNN@<machine>` (for example
  `Zeke_projects-foo_002@arch`): the path from ~ keeps same-named folders apart
  and the machine only labels, so numbering and "last session" work across
  computers. Old names still count. (claude-tools)
- The Claude tools (`claude-new`, `claude-pick`, `claude-fresh`) moved to the
  public repo claude-tools, and `webapp` to the public repo webapps, both with a
  README and an install.sh for pacman, dnf and apt, so the Arch desktop can use
  them without the Fedora-only parts of the dotfiles. My list of apps is now
  webapps/.config/webapps/apps.conf here, and `webapp sync` writes the launchers.
  `claude-fresh` also updates claude-tools and the repos in
  claude/.config/claude-tools/repos when Claude is opened in ~.
- `claude-fresh`: before Claude starts in a folder, the project is brought up to
  date from GitHub if it is only behind and clean (`git pull --ff-only`); in ~
  it is ~/dotfiles. `claude-pick` marks folders with ↑ not pushed, ✎
  uncommitted, ↓ behind.
- `claude-pick` (Super+a): rofi launcher for Claude Code. Folder first (~ is the
  default, then folders already worked in, then ~/Zeke_projects), then session;
  shows the context usage of each and suggests a new session with a handoff
  from 50%. `claude-new` got `--new` and `--last`.
- Mousepad: the Catppuccin Mocha scheme never loaded (GtkSourceView needs
  `version="1.0"` in the XML), so the current line and the gutter were white.
- GTK popup menus are flat (gtk.css): no rounded box with shadows.
- Panel web apps and Calendar start on a new tab and no longer restore the
  previous session (`webapp fresh`): Brave restored one tab and the launcher
  added another, so WhatsApp asked "use here".
- The toolbar crop of panel web apps works in any tile position (not only at
  the top edge): such windows are stacked below the others.
- Opening a panel web app (or Calendar) that is already running now raises
  its window instead of opening another tab; extra WhatsApp/Messages tabs
  made the site ask "use here".
- Google Messages is a `webapp --panel` app (own profile, Claude side panel);
  it replaces the old brave --app launcher.
- Google Calendar is a tiled `WebPanel-calendar` window like the panel web
  apps (Brave's toolbar pushed off the screen by xmonad) instead of fullscreen.

## 2026-10-06
- picom no longer blurs the screenshot area selector (slop window), so the
  screen stays sharp while choosing the area.
- `webapp add ... --panel`: own Brave profile, tiled window with the Claude side panel (Ctrl+E);
  xmonad pushes Brave's tab strip and toolbar off the screen (WebAppCrop). WhatsApp uses it.
- `webapp`: turn any website into an app window with a rofi launcher (Brave
  `--app`, shared profile, no keyring prompts); google-calendar also skips
  the keyring now; `webapp extensions` opens the shared profile to install
  FireShot (full-page PDF/PNG).
- Mousepad as the default text editor, with Catppuccin Mocha colours (it
  was LibreOffice Writer for plain text).
- micro, a terminal editor with Ctrl+S/C/V shortcuts, Catppuccin Mocha.
- Numbered Claude Code sessions (`claude-new`) and ~/CLAUDE.md.
- Printer addresses moved out of the public repo to ~/.config/printers.env;
  Claude's state file is per machine (~/.claude/STATE.local.md).
- `claude-new` works per folder (`claude-new [folder]`, named <machine>_<folder>_NNN):
  it shows the last session there and asks to continue it or start the next.
- docs/: system state, changelog and decision log.

## 2026-10-05
- Printers: Brother MFC-L2700DN (driverless) and Canon iX6800 (through
  hs-media-srv); Brother scanner in simple-scan through sane-airscan.
- Google Calendar app: own Brave profile, fullscreen, Claude extension;
  Super+Shift+C opens it and Super+Ctrl+C its extensions. Close-window keeps
  only Super+W.
- Mini calendar on the bar's date: own GTK grid, Google Calendar events with
  a pink dot on busy days, "Ask Claude…" box to add or change events.
- Battery script with smoothed time left; colour-coded CPU, memory and
  battery icons; cleaner Wi-Fi module with network details on click.
- Dropdown terminal: translucent with blur, slides in and retracts up.
- Screenshot actions and user-defined custom actions in the shortcut editor;
  Super+L restored for the lock screen.

## 2026-10-04
- Shortcut system: action catalog in xmonad.hs, keys.conf, rofi shortcut
  editor (keys-editor), cheat sheet (keybinds) that runs shortcuts, tests.
- Android integration (KDE Connect, scrcpy, OTP to clipboard), OSD for
  volume and brightness keys, Thunar bookmark for ~/Zeke_projects.
- LazyVim for Java, Erlang, Elixir, TypeScript and front end.
- Alacritty as default and dropdown terminal, starship prompt.
- Fedora post-install script (codecs, VA-API, snapper, power profiles).
- All user-facing text and comments translated to English.

## 2026-10-03
- XMonad and Polybar setup ported to Fedora with one bar per monitor.
- Catppuccin Mocha Pink for GTK, Qt, icons, cursor, rofi, dunst and CopyQ.
- Dropdown terminal with persistent tmux sessions, visible lock screen,
  automatic timezone, XDG folders, Brave and Atril as defaults.

## 2026-01 to 2026-05 (previous machine)
- First XMonad, Polybar and Qtile configs; multi-monitor bar launcher,
  caffeine toggle, per-monitor window titles.
