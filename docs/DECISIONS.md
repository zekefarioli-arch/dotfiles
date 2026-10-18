# Decisions

Choices between alternatives, with the reason, so they are not argued again
or undone by accident. Newest first. Each entry: what was decided, what else
was considered, why. Small choices inside a file are explained in a comment
next to the code instead; this file is for choices that shape the system.
If a decision is reversed, add a new entry and mark the old one "Replaced".

## 2026-10-06 · Private and per-machine values stay out of the repo
Decided: printer addresses live in ~/.config/printers.env (not in the repo);
each machine has its own state file, linked as ~/.claude/STATE.local.md,
which ~/CLAUDE.md imports. Considered: keeping the addresses as defaults in
the script (still public); one STATE.md for both computers (describes the
wrong hardware on one of them); a `.env` committed encrypted (more tooling
than the problem needs). Why: the repo is public and the two computers
differ. The old addresses stay in git history; they are private LAN
addresses, so the history was not rewritten.

## 2026-10-06 · Session names per folder, numbered from the saved sessions
Decided: `claude-new` names a session <machine>_<folder>_NNN (folder is
`home` in ~) and takes the next number from the sessions Claude already saved for that
folder; if one exists it shows the last and asks to continue or start a new
one. Considered: a global counter file (the first version), which ignores
what the folder is and cannot be shared between computers without Git
conflicts. The machine part is the first 4 characters of /etc/machine-id
(unique per install, where hostnames may repeat), or the text of
~/.config/claude-new/machine for a readable name such as "arch".
Why: most work happens in a code project, and the folder is what tells
sessions apart. Sessions are stored per machine and never shared; what travels is dotfiles, the docs and the projects on GitHub.

## 2026-10-06 · Fresh Claude Code sessions with a state file
Decided: start a new numbered session (`claude-new`) every so often, with
~/CLAUDE.md, docs/STATE.md and Claude's memory carrying the context.
Considered: one long session. Why: long sessions get slow and less precise;
a fresh one with a short, true state file answers better.

## 2026-10-05 · Scanner through sane-airscan, not Brother's brscan4
Decided: scan over WSD with sane-airscan, scanner pinned by address.
Considered: Brother's brscan4 driver. Why: brscan4 is an unsigned RPM that
Fedora refuses by default; airscan works without it. The auto-discovered
entry failed to open at times, so the address is pinned and that entry
blacklisted.

## 2026-10-05 · Printers: Brother direct, Canon through the server
Decided: print to the Brother at its own address (driverless IPP), and to
the Canon through the CUPS server it is plugged into; cups-browsed off.
Considered: both through the server's shared queues. Why: the Brother is on
the network by itself, so going direct is faster and does not depend on the
server; the Canon needs the server's driver. cups-browsed would add
duplicate queues.

## 2026-10-05 · Google Calendar app as a fullscreen Brave profile
Decided: a separate Brave profile (own window class, extensions, login and
theme) opened fullscreen. Considered: `brave --app`, Flathub web app tools
(Web App Hub, Web Apps, Quick Web Apps). Why: app windows have no side
panel, and the Claude extension needs one plus normal windows for its tab
groups; the Flathub tools create app windows or use WebKit (no Chrome
extensions) and need a large runtime.

## 2026-10-05 · Calendar events from secret iCal addresses
Decided: read events from Google's secret iCal addresses; changes go
through the "Ask Claude…" box (Claude Code with the calendar connector).
Considered: Google Calendar API with OAuth. Why: iCal needs no Google Cloud
project and is read-only, so it cannot break anything; OAuth is deferred.
The addresses work as passwords, so they live only in
~/.config/mini-calendar/ics-url (mode 600).

## 2026-10-05 · Ask Claude through headless Claude Code
Decided: `claude -p` with stream-json, warmed up when the box gets the
focus, retried if the calendar tools are missing; only calendar tools
allowed. Why: claude.ai connectors connect a few seconds after the session
starts; the prompt goes on stdin because --allowedTools would swallow a
positional prompt.

## 2026-10-05 · Own GTK month grid for the mini calendar
Decided: draw the month with plain GTK widgets and CSS. Considered: yad
--calendar, GtkCalendar. Why: yad always keeps a day selected (so other
months highlight a day), and GtkCalendar draws a line under every day once
day details are on. It does not close on focus loss, because focus follows
the mouse.

## 2026-10-05 · No simulated input in the live session
Decided: never drive the X session with xdotool key/type/click; test logic
headlessly and ask me to try the UI. Why: simulated keys switched me to a
text console (Ctrl+F5 became Ctrl+Alt+F5), typed into the browser and went
into i3lock as wrong passwords, after which the session would not unlock.

## 2026-10-05 · Dropdown hides on shortcuts, not from logHook
Decided: hide the dropdown before rofi, Super+Enter and Super+E.
Considered: auto-hide on focus loss from xmonad's logHook. Why: toggling a
scratchpad from logHook put xmonad in a busy loop; nsHideOnFocusLoss has no
effect with this setup.

## 2026-10-04 · Shortcuts in keys.conf over an action catalog
Decided: xmonad.hs defines named actions; keys.conf maps them to keys and is
read at startup. Considered: bindings written in Haskell. Why: changing a
shortcut needs only `xmonad --restart`, a rofi editor and a cheat sheet can
work on plain files, and bad lines produce a notification instead of a
broken build.

## 2026-10-04 · Replay shortcuts by keycode
Decided: the cheat sheet replays shortcuts with keycodes. Considered:
`xdotool key` with key names. Why: by name, xdotool added Alt to F-keys and
switched to a text console.

## 2026-10-04 · Alacritty for the terminal and the dropdown
Decided: alacritty with tmux. Considered: terminator, wezterm. Why: lighter
and faster, and Shift+Enter / Ctrl+Enter reach tmux and Claude Code.

## 2026-10-03 · GNU Stow with --no-folding
Decided: every config is a Stow package installed with --no-folding.
Why: links only files, so programs can still write their own files next to
mine without them landing in the repository.

## 2026-10-03 · Bars relaunched from scratch on monitor changes
Decided: kill all bars and start one per connected monitor each time.
Why: no state to get wrong; no bar is left behind for a monitor that is gone.

## 2026-10-06: web apps with a small script, no keyring
- Decided: `webapp` script, `brave --app=URL` in one shared profile with
  `--password-store=basic`.
- Why: I do not want the keyring password prompt, and I want nothing new
  installed. Basic store keeps cookies in the profile instead of the keyring.
- Considered: webapp-manager (GUI, pulls GTK/Python deps), Nativefier/Electron
  (a Chromium per app, ~200 MB each), unlocking the keyring at login (needs
  PAM setup, the behaviour I want to avoid). Google Calendar stays a separate
  profile because it needs the Claude extension (see its script header).

## 2026-10-06: hiding Brave's toolbar in panel web apps
- Decided: a layout modifier (WebAppCrop) that extends the rectangle of a
  WebPanel-* window upwards by ~86 px and stacks these windows below the
  others: the tab strip and toolbar fall off the screen at the top edge, or
  under the window above anywhere else. The window stays tiled.
- Why: the Claude side panel needs a normal window, which always shows Brave's
  toolbar; it has to be hidden from outside.
- Considered: --app (no side panel); Brave fullscreen (hides the UI but covers
  the bar and other windows, and a hook to sink it again fought two EWMH
  handlers: tried and replaced, commit b70672e); floating window offset
  upwards (leaves the layout); a Chromium patch (too heavy).
- Google Calendar follows the same rule (class WebPanel-calendar, no
  fullscreen) since its fullscreen did not fit xmonad's Full layout.
- Limits: height is a constant to tune. A first version only cropped windows at
  the top edge, and a web app in the second row showed its toolbar again.

## 2026-10-07: no session restore in web app profiles
- Decided: before each launch, if the profile is not running, `webapp fresh`
  sets session.restore_on_startup = 5 (new tab page) and exit_type = Normal in
  the profile's Preferences. Used by webapp and google-calendar.
- Why: Brave restored the previous tab and the launcher added its URL as a
  second one; WhatsApp and Messages allow one session per profile and asked
  "use here". Seen in the session files (tab 0 restored, tab 1 from the URL).
- Considered: dropping the URL when restoring (the first start has nothing to
  restore); a managed policy (RestoreOnStartup) in /etc, which is system-wide
  and would also hit the normal browser. Closing the extra tabs by hand does
  not last.
- Limit: Brave rewrites its preferences on exit, so it only works while the
  profile is not running.

## 2026-10-07: flat GTK menus through ~/.config/gtk-3.0/gtk.css
- Decided: override the theme's popup rules in gtk/.config/gtk-3.0/gtk.css
  (no radius, no margin, no drawn shadow, 1px border).
- Why: Catppuccin-Pink-Dark draws menus as a 12px rounded box inside a
  transparent 20px margin with a shadow, which looked like a square box with
  shadows around it (Mousepad's menu bar).
- Considered: editing the theme itself (lost on updates, outside the repo);
  picom rules (the shadow is drawn by GTK, not by picom).
- Not verified on screen: popup menus cannot be opened without simulating
  input; check it by opening Mousepad's File menu.

## 2026-10-07: Claude launcher in rofi, with context usage
- Decided: `claude-pick` (Python, stdlib only) opens rofi lists for folder and
  session, shows the context of each session (last reply's prompt tokens over
  the window) and suggests a new session with a handoff from 50%. `claude-new`
  stays the one place that names and numbers sessions.
- Why: choosing by hand where to run Claude and when to cut a long session was
  guesswork. The context size is in every reply Claude saves, so it costs
  nothing extra to show it.
- Considered: fzf in the terminal (one more package, and the folder tree and
  colours are nicer in rofi, which I already use everywhere); extending
  `claude-new` itself (one tool, one job); file size or number of messages as
  the metric (they say little: a 28 MB session can still be compacted).
- Limits: the window size is an assumption (200000, or 1000000 if a session is
  above it); the percentage drops by itself after a /compact.

## 2026-10-07: update the project when opening it on the other computer
- Decided: `claude-fresh` (shared code in lib/claude-tools/gitfresh.py) runs
  before every session: 4 s `git fetch`, then `git pull --ff-only` only if the
  tree is behind and clean. Dirty, diverged or ahead: warn and wait for Enter,
  touch nothing. Never merge, force or push by itself. In ~ it works on
  ~/dotfiles. claude-pick shows ↑ ✎ ↓ per folder from what git already knows.
- Why: the two computers share the projects through GitHub, so opening one
  that the other pushed to would have Claude working on old code.
- Considered: asking in rofi before updating (one more click every time); only
  marking the folder (the update stays manual and gets forgotten); auto-stash
  or rebase (can hide or rewrite my work); syncing the folder with Syncthing
  (conflicts, and git already is the source of truth).
- Limits: it needs the network for the fetch (skipped without it); the ↓ badge
  is as of the last fetch, so it is only exact right after opening a project.

## 2026-10-07: the Claude tools and webapp live in their own repos
- Decided: `claude-tools` (claude-new, claude-pick, claude-fresh) and `webapps`
  (webapp) are separate public repos in ~/Zeke_projects, installed by an
  install.sh that links the commands into ~/.local/bin. The dotfiles keep the
  Super+a action, my list of apps (webapps/.config/webapps/apps.conf), the extra
  repos for claude-fresh and the xmonad crop rule. In webapp the apps are a
  list file and the launchers are generated (`webapp sync`) instead of being
  stow-managed .desktop files.
- Why: the dotfiles mix things that only make sense on the Fedora laptop
  (scripts/, system/, STATE.md, laptop scripts in bin/) and the other computer is
  an Arch desktop, so cloning them there would bring all of it. The tools have
  nothing personal and are worth documenting once, for any machine.
- Considered: a new stow package in the same repo (smallest change, but the
  Arch machine still clones the whole mix); reorganising the whole dotfiles by
  machine (too big, touches everything that works); a private repo (the tools
  hold no secrets, and my personal data stays in the dotfiles and ~/.config).
  For webapp, keeping launchers as stow files would tie it to ~/dotfiles.
- Limits: a change to a tool is now two commits in two repos when it also
  touches the dotfiles; claude-fresh in ~ updates them all, so the other
  computer follows on its next session.

## 2026-10-07: session names with the path and the machine as a label
- Decided: `<path>_NNN@<machine>`. The path is the folder from ~ with dashes; the
  machine goes after the `@` and is ignored when looking for the last session
  (the most recent one wins) and for the next number (highest plus one).
- Why: the last folder name alone mixes `~/work/app` and `~/Zeke_projects/app`,
  and a name that carries the machine in front made the sessions of the other
  computer invisible. With the machine as a label, a folder shows the work of both
  computers and the numbers do not collide if their sessions are ever together.
- Considered: the path without the machine (two computers can both create `_001`
  and clash); keeping `<machine>_<folder>_NNN` (does not help to share); dropping
  `Zeke_projects-` from the path of my projects (shorter, but I asked for the path
  from ~ and it keeps one rule for every folder).
- Limits: two different paths can give the same name (`a/b` and `a-b`); the
  sessions themselves are still not shared between the computers (next step).

## 2026-10-07: no automatic screen lock
- Decided: remove `xss-lock` from the session. The screen is locked only by hand, with
  Super+L (`lock-screen`, i3lock).
- Why: xss-lock ran `lock-screen` on every suspend and lid close, so each time I opened
  the lid I met a password prompt. The logs showed the lock came from the lid and the
  suspend, not from inactivity (the X screen saver was already off).
- Considered: keeping the lock only on suspend but not on the lid (a closed lid suspends
  the laptop anyway, so it changes nothing); a longer delay (there is no delay, it locks
  before sleeping); locking only on battery. The cost is that anyone who opens the lid
  of a suspended laptop finds the session open, which matters most away from home.
- To restore it: uncomment the `xss-lock` line in xmonad/.xmonad/autostart.sh and start
  it once. The `lock-screen` script stays.

## 2026-10-07: a git hub on the home media server, each machine pushes to two places
- Decided: bare repositories on the media server, owned by a restricted `git` user whose shell
  is `git-shell`, reached over ssh. Every clone has `origin` on the hub with two push URLs, the
  hub and the public copy (GitHub, or GitLab for the private repo), and a spare remote for
  away from home. The tests keep running on the laptop and the Arch desktop.
- Why: one central copy at home that every computer reaches quickly and without the internet
  (claude-fresh fetches from it), and a second, off-site copy at no cost; GitHub also failed
  to create private repositories on the day I did this. A repository that had no remote at
  all (a private one on the Arch desktop) got its first copy.
- Considered: the router as the server (more disk, but it is the machine exposed to the
  internet and the one that must not break); reusing my own user (it is in the `docker` group,
  which equals administrator, so every added key would grant that); the server pushing to
  GitHub and GitLab by itself with a hook (it would need their tokens stored on the server);
  Forgejo or Gitea (a web page and more to maintain; it can be added later on top of the same
  repositories); moving every repo at once instead of five.
- Limits: the hub has no backup of its own; away from home it needs the VPN, and the spare
  remote covers that; a push away from home reaches only the public copy and reports an error
  for the hub.

## 2026-10-12: Gemini from Claude Code through the Antigravity CLI, with the Gemini CLI as fallback
- Decided (later the same day): `agy`, the Antigravity CLI, is the default, because it runs on the Google AI
  Pro plan the user already pays for, reads files with its own tools and returns JSON headless
  (`agy --print ... --output-format json --mode plan < /dev/null`); stdin text is not read, so inputs go in
  files. The Gemini CLI below stays as the fallback on the API key.
- Decided first: Google's Gemini CLI (`@google/gemini-cli`, installed by npm with `--prefix ~/.local`,
  no sudo and no system package) authenticated with an AI Studio API key kept in the system
  keyring, and a user skill `gemini` in the `claude` package that tells every Claude Code session
  how to call it headless (`gemini -p ... -o json --approval-mode plan`, files by `@path`).
  Claude calls it from Bash like any other tool, in any project.
- Why: a second model that did not write the content catches wrong keys and ambiguities the
  author cannot see (it found 15 items in the young learners mocks on 2026-10-11); the CLI is
  one maintained command, reads files, returns JSON and needs no code of ours. The Google-account
  free tier stopped serving the CLI the same day ("migrate to Antigravity"); the key route was
  verified to work and costs cents at this volume.
- Considered: a community Gemini MCP server (a typed tool for Claude, but third-party code and
  dependencies to trust; can still be added on top of the same key if a tool feels better than a
  command); Composio or another hosted MCP gateway (prompts and code pass through a third party,
  plus an account and a plan); driving Antigravity, which is installed (its internal `agentapi`
  only works inside a running IDE session and keeps the answers in the IDE's own database);
  the Code Assist project and Vertex AI modes of the CLI (both answer "no valid licence" without
  a paid plan); a script of our own against the Gemini API (more to maintain for no gain).

## 2026-10-14: CI pipelines for private repos: cut the minutes first, a runner only on a dedicated machine, never on the router
- Decided: GitHub stays the home of the code and the pull requests. When the free Actions minutes
  (2,000 a month on the Free plan) run out, the first step is to spend fewer: end-to-end tests only
  on `main`, no CI for docs-only changes, fewer Dependabot pull requests. If that is not enough, run
  the same workflows on a self-hosted GitHub Actions runner (`runs-on: self-hosted`) in a container on
  a dedicated machine: ephemeral (one container per job), with CPU and memory limits and no route to
  the rest of the LAN. Deploy jobs stay on GitHub's own runners, so the server keys never reach the
  house. The spare laptop (inspiron) is the first candidate; a second 16 GB module would help.
- Never on the router (hs-gateway): it does PPPoE, NAT, DHCP, DNS and the VPN for the whole house, so
  a full Gradle and Playwright run would starve the network, Docker rewrites firewall rules there, and
  a PR's code would run on the machine that every packet crosses. It already runs a few containers;
  that is not a reason to add heavier ones. This reverses the line "CI, long builds" that the
  machines inventory listed for it; it agrees with the 2026-10-07 decision, which kept the git hub
  off the router because it "must not break".
- Considered: moving the repo to GitLab (400 free minutes, rewrite the pipelines, Dependabot has no
  equivalent there, pull requests do not migrate); a GitHub Enterprise trial (the repo would have to move
  into an organisation, and it asks for a card); making the repo public (free minutes, but private
  notes and content live in it); a separate CI server such as Woodpecker or Jenkins (own pipelines to
  rewrite and another service to maintain); paying for Pro ($4, only 1,000 more minutes).
- Open: whether GitHub lets a self-hosted runner start while the Actions billing notice is on (should
  be, since it uses no minutes; to be tested). Nothing is installed yet.

## 2026-10-14 (later): the CI runner goes on the new Ryzen desktop, in a Debian virtual machine
- Decided: when the new desktop arrives (Ryzen 9 9900X, 12 cores and 24 threads, 16 GB DDR5 now and 32 GB later,
  1 TB, two Ethernet ports) it is both my workstation and a 24/7 machine. The host runs Fedora for work.
  The GitHub Actions runner lives in a Debian 13 virtual machine (KVM) on it: about 6 threads and 8 GB, lower
  CPU priority than the host so work stays smooth, one runner and one job at a time, deploy jobs still on
  GitHub's runners. The suspend setting must be off while it serves CI. Until it arrives the first step stays
  the same: spend fewer GitHub minutes.
- Why: a Debian guest gives the CI the environment it was written for (`apt`, Playwright `--with-deps`, Docker
  without SELinux or firewalld surprises), and the code of pull requests and dependencies runs apart from my
  files and sessions; if anything goes wrong the VM is deleted and rebuilt. Fedora on the host keeps
  newer packages for development, which Debian stable would not.
- Considered: the runner directly on the Fedora host (needs three workarounds: Playwright `--with-deps`, SELinux
  `:z` on mounts, firewalld with Docker, and runs untrusted code beside my files); Debian on the whole machine
  (old packages for daily work); the router, the media server (i5-4570T, 4 threads, a 3.9 GB /var, 9 media
  services) and the spare laptop (see the entries above; they stay rejected).
- Supersedes the "first candidate: the spare laptop" line of the entry above; that one stays as history.

## 2026-10-16: the laptop moves from Fedora 44 to Ubuntu Server 26.04 LTS
Decided: replace Fedora on the ThinkPad with Ubuntu Server 26.04.1 LTS (minimized, text
installer) and build the same desktop on it from these dotfiles. The plan, the package map
and the checklists are in docs/UBUNTU-MIGRATION.md. The old 236 GB disk stays untouched as the
way back, and a backup of everything changed since it was copied is on the gateway.
Considered: staying on Fedora (a release is supported for about 13 months, so it means an
upgrade or reinstall every year); Ubuntu Desktop (brings GNOME and snaps, against the light
system I want); cloning the Fedora disk onto the new install (different distribution, so it
cannot work); an in-place conversion (not possible). Why: I asked for the latest stable LTS, which has
five years of security updates, on a base that is as small as the Fedora one. Filesystem: ext4 (chosen
on 2026-10-16: the installer makes it without manual steps, and the dotfiles plus the backups cover what
snapshots did). Still open: whether picom 12.5, the Ubuntu version, accepts
every line of the picom.conf (the rules syntax is from picom 12, so it probably does).

## 2026-10-16 (later): the router may hold files I ask it to hold, nothing more
Decided: `hs-gateway` stays read-only for Claude by default, but Claude may write files in
zeke's home directory there when I ask for that task (the first use: the backup of the laptop for
the move to Ubuntu, in ~/migration-2026-10-16). Everything else of the 2026-10-14 rule stands: no
services, CI runners, build jobs or containers, and no changes to its firewall or Docker setup.
Considered: keeping it strictly read-only (the backup would have needed another machine, and the
only one with free space and no role in the network is the router's own /home partition, 596 GB
free, separate from the system disk); allowing any change on it (the reasons of 2026-10-14 still
hold: a heavy job would starve the network of the whole house, and Docker rewrites firewall rules
there). Why: the risk the rule guards against is load and broken routing, and a static file in
a user's home adds neither.

## 2026-10-17: the laptop moves to Ubuntu 26.04; NetworkManager is switched with a reboot, not `netplan apply`
- Decided: Ubuntu Server 26.04 (minimized) plus the dotfiles desktop, built by scripts/ubuntu-post-install.sh and the
  backup on the gateway, driven over ssh from the Arch desktop (the plan said the gateway, but the router should run
  nothing, so it only served the backup). Kept from the earlier plan: ext4, no snaps, ufw, NetworkManager.
- NetworkManager takes over the network only by a reboot. Two attempts with `netplan apply` while systemd-networkd
  and NM both ran left two wpa_supplicants fighting for the card and the laptop off the network. What worked: write the
  NM netplan file, disable systemd-networkd, enable NetworkManager, reboot, with a boot-time fallback unit that
  restores the old netplan if there is no default route after 4 minutes.
- Restoring Brave profiles copies Chromium's `SingletonLock` (it names the old host); delete the `Singleton*` links or
  the browser waits forever (it blocked `findajob`).
- Cloning the old Wi-Fi MAC to get the reserved address `.20` back did not work, so the laptop keeps a new address.
  To get a fixed one, change the router reservation to the new MAC (c0:3c:59:ed:84:f1).

## 2026-10-17: `machine`, a tool for the name and the fixed address; reservation in the router, not a static address
- Decided: a small tool (bin/.local/bin/machine) because the disk moves between computers. The address is a DHCP
  reservation in the router (dnsmasq, runbook dhcp-reservation in home-infra); the tool picks a free number, refuses
  the taken ones and prints the command, and never writes to the router (hs-gateway stays read-only for Claude).
- Considered: a static address in NetworkManager on the machine (works without the router, but can clash with the
  pool or another machine and spreads address management over every computer). Rejected.
- The taken numbers and the pool live in the private repo home-infra (config/machine-network.conf), linked as
  ~/.config/machine/network.conf, because dotfiles is public and the home addresses are private; the tool refuses to pick
  a number if the list is missing. The pool range is refused too, so a reserved number is never handed out.
- A reservation is by MAC: when the disk goes to another computer the network card changes, so the tool has to be run
  again there. Numbers .20 and .21 stay refused: they are still reserved in the router for the old MACs.

## 2026-10-17: CopyQ without a tray icon in Polybar
- Decided: leave it. CopyQ 13 is built on Qt6, which only speaks StatusNotifier, while Polybar's tray is XEmbed only; Ubuntu has no
  `snixembed` package. It works the same through `Super+V` (`copyq toggle`).
- Considered: building snixembed from source (needs valac and GTK dev packages just to compile; against keeping the system light).

## 2026-10-17 (later): the CopyQ tray icon did work; the earlier entry is wrong
- Reversed the entry above: CopyQ docks in Polybar's XEmbed tray (it showed as a 21x21 client) once it is restarted, so
  there is no need for snixembed and the Qt6/SNI explanation was a wrong guess, made without testing a restart first.
  The icon was most likely lost because Polybar was relaunched by hand after CopyQ had started.
- If the icon is missing after a login, the cause is the start order (CopyQ before the bar's tray). A fix would be to
  start CopyQ after the bars in autostart.sh; not done yet, to see whether it happens.

## 2026-10-17: try Hyprland in a separate admin user, xmonad stays as it is
- Decided: Hyprland is installed system-wide (apt, 30 small packages, no recommends) but its configuration lives only in the
  user `zeke-hypr` (administrator, in groups sudo and video), in a stow package `hypr` on the branch `hyprland` of that user's
  clone of dotfiles. At the login screen I choose the user; `zeke` keeps xmonad, polybar and picom exactly as they are.
- Considered: a nested Hyprland in a window of the xmonad session (xmonad grabs Super, so the shortcuts could not be tried
  as they are); the same user with another session (a half-ported config could break `zeke`'s files). Rejected.
- Theme: Catppuccin Mocha with the pink accent as a palette file (mocha.conf, from the same colours as catppuccin/waybar,
  because catppuccin/hyprland no longer has the file at its old path). Full rices (HyDE, end-4, JaKooLit) not installed:
  they pull dozens of packages and overwrite configs, against keeping the system light; to be looked at only if I ask.
- Layout mapping: Tall = master (left), Mirror Tall = master (top), ThreeColMid = master (center), Full = maximize.
  Grid and my own F1Layout have no equivalent. Hyprland 0.55 deprecates hyprlang for Lua: this config is for 0.53.

## 2026-10-17: the disk moves to the Ryzen 9 desktop; name `zeke-ryzen9-desktop`, address `.23`
- Decided: hostname and `claude-new` label `zeke-ryzen9-desktop` (set with `machine name ... --hostname` plus the 127.0.1.1 line of
  /etc/hosts). Fixed address `.23` as a DHCP reservation by the desktop's wired MAC, next to the other fixed machines (.20-.22).
- Considered: keeping `.140` (it is inside the DHCP pool 50-200, so the router could hand it out; the tool refuses it); `.24` and
  `.30` (also free, no reason to prefer them). Kept the label equal to the hostname so there is one name to remember.
- The reservation was run by hand (the router is read-only for Claude) with the command printed by `machine ip 23`, then
  `machine ip --renew`: the desktop has 192.168.0.23. 23 is now in `reserved=` in home-infra/config/machine-network.conf.
- The old laptop MAC reservations (.20, .21) stay as they are, as decided on 2026-10-17 above.

## 2026-10-17: the ThinkPad gets its own state file, STATE-thinkpad.md
- Decided: STATE.md stays the Ryzen 9 desktop's (the 1 TB Ubuntu disk went there), and the ThinkPad, back on its 256 GB Fedora
  disk and renamed `zeke-thinkpad-x13`, has docs/STATE-thinkpad.md, linked as ~/.claude/STATE.local.md. Same layout as STATE-arch.md:
  what is specific to the machine, and a pointer to STATE.md for the shared desktop.
- Considered: keeping the laptop in STATE.md (sessions here would read another machine's hardware and network); copying the whole
  STATE.md (two long files to keep equal). Rejected. The STATE.md "Pending" still carries a few ThinkPad items from before the
  move; they are repeated in the new file and can be cleaned up there later.

## 2026-10-17: Alacritty font size per machine, in an untracked local.toml
- Decided: alacritty.toml no longer sets `font.size`; it imports ~/.config/alacritty/local.toml (outside the repo), where each machine
  sets its own (ThinkPad 11.0, Ryzen 9 8.5). dropterm.toml imports alacritty.toml, so it inherits the size. A missing local.toml is skipped.
- Why not just override in local.toml while keeping a size in the shared file: Alacritty loads imports first and the importing file last,
  so the shared value would win. The size has to be absent from the shared file.
- Considered: lowering or raising the shared value (it just moves the problem to the other machine); editing the stowed file by hand
  (the next pull overwrites it). Rejected. A machine with no local.toml gets Alacritty's default size (11.25).

## 2026-10-17: the CI runner is a persistent runner in an Ubuntu Server 26.04 VM on the Ryzen 9 desktop
- Context: the free GitHub Actions minutes ran out (2,000 a month) and I could not test any saving, because Actions was blocked. Only the
  private repo `mock-oclock` spends them: its CI ran 167 times in the first 17 days of October, five jobs of about 3 minutes each,
  billed per job rounded up. This puts the 2026-10-14 plan into practice.
- Decided: a KVM virtual machine, `ci-runner` (6 vCPU, 6 GB, 80 GB, low CPU priority, autostart), with a GitHub Actions runner as a
  systemd service. The five CI jobs take `runs-on` from the repository variable `CI_RUNNER` and fall back to `ubuntu-latest`, so the
  runner can be switched off without a code change. `deploy.yml` stays on GitHub. GitHub did start the runner while the Actions billing
  notice was on, which was the open question of 2026-10-14; the five jobs passed on it in about 5 to 6 minutes, one at a time.
- Guest OS: Ubuntu Server 26.04 LTS, not the Debian 13 of the 2026-10-14 plan. Debian was built first and worked, but the CI is written for
  `ubuntu-latest` and Debian needed three patches (the `docker compose` plugin, pip's PEP 668 block, Playwright's dependency list);
  Ubuntu needed none, and it is the same release and kernel as the host. Considered: Ubuntu 24.04 (closer to `ubuntu-latest`, but older
  and no reason to prefer it once 26.04 passed the whole CI). The Debian VM was deleted.
- Persistent runner, not ephemeral: an ephemeral runner needs a GitHub token inside the VM to register itself again after each job, and
  that is where the code of pull requests runs. A persistent one only holds its own runner credential, and the VM can be rebuilt. Revisit
  with a controller on the host if it ever matters. Only for private repos: never for forks of public ones.
- Hooks, not workflow changes: the e2e job's `docker compose` leaves root-owned files in the workspace and the next `checkout` failed. A
  runner hook (job started and completed) fixes the ownership and removes leftover containers, networks and volumes, so the repository
  stays the same as on GitHub's runners.
- Seeing it: ttyd (current step) and Glances (CPU, memory, containers) as web panels on demand through an ssh tunnel, not as services.
  Cockpit and Netdata were rejected: always-on services and many more packages. Glances comes from PyPI because the Ubuntu package ships
  without its web files.
- Boot detail worth remembering: the Debian cloud image looped in GRUB with BIOS boot on this CPU (any CPU model); UEFI and q35 fixed it.
- Not on the router, as decided on 2026-10-14 and recorded in ~/CLAUDE.md.

## 2026-10-17 (later): the CI runner's web panels are always on, behind a password, served by the Ryzen
- Decided: the live view of the runner (ttyd) and of the VM's load (Glances) are two systemd services in the VM, listening only on the VM's address in
  libvirt's NAT network, and nginx on the Ryzen proxies them to the home network (the ttyd and Glances default ports) with basic authentication. ufw
  lets only the home network reach those two ports. The VM's address is reserved in libvirt so the proxy keeps working after a reboot.
  This replaces the first design of the same day, an on-demand ssh tunnel to `localhost` on the laptop: it needed a command and a terminal each time,
  and a suspended tunnel (Ctrl+Z) or a cut one left the pages dead.
- Why: I want to open the two pages whenever I like, from any device at home, and see nothing when there is nothing to see. The cost is small: about
  90 MB of RAM and 1 to 2% of a core for Glances in the VM, a few MB for nginx on the Ryzen.
- Considered: keeping the tunnel as a permanent user service on the laptop (works only on that laptop); a bridge so the VM has its own LAN address (puts
  a runner that executes pull requests' code on the LAN, against the isolation of 2026-10-14); `systemd-socket-proxyd` (no password: Glances shows every
  process and ttyd shows job output); Caddy (about 50 MB for the same job); Cockpit and Netdata (always-on, many more packages). nginx-light is four small packages.
- Accepted risk: basic authentication over plain HTTP on the home network, so the password is visible to anyone who can sniff my LAN. It protects a read-only
  monitoring page; the password is not to be reused. The password is set with `ci-panels-passwd` on the Ryzen and never goes through a chat or a command line.
- Fails closed: an empty password file lets nobody in. Not tried over the VPN.

## 2026-10-17 (later still): the CI runner VM serves a second private repo, with a lock between the runners
- Decided: `due-card` (Elixir and Phoenix, one CI job against PostgreSQL) uses the same VM through a second runner, because it also needs the free minutes
  that ran out and a personal account cannot share one runner between repositories. Its `ci.yml` reads the runner from `CI_RUNS_ON`, as in `mock-oclock`.
  That repo works on `main` with no pull requests (its ADR 0002), so the change was first validated with a manual run on a throw-away branch and then the
  same commit went to `main`, which stayed green.
- A **VM-wide lock** in the runners' job hook: one job at a time in the VM, whichever repository it belongs to. Without it the cleanup that runs between jobs
  (remove every container) would kill the containers of a job running for the other repository, and the two would share Docker and ports (`due-card`
  publishes the Postgres port of the host). The hook tells the start from the end of a job by who holds the lock, so one file serves both and could be swapped
  in place while a job was running, with no runner restart.
- Considered: a second VM for `due-card` (about 4 GB less for the desktop, more to maintain); no lock and run both at once (cleanups kill the other's containers,
  and Gradle plus Playwright plus Elixir at once do not fit in 6 GB); waiting until `due-card` burns minutes again (it had none this month). Rejected.
- Cost: jobs of the two repos wait for each other. Dependabot adds up: touching its configuration made four pull requests refresh at once and the queue of about
  25 jobs took half an hour to drain.
- `erlef/setup-beam` needs `ImageOS=ubuntu24` in the runner's environment on a self-hosted machine; it passed the first time on Ubuntu 26.04.

## 2026-10-17: model routing with agents that carry their model, not an MCP and not a rule to remember
- Decided: the full-stack blueprint and frontend-kit route work to the cheapest model that does it through **agents whose file names the model** (`model: haiku`,
  `sonnet` or `opus` in the frontmatter, which is what Claude Code reads): mechanical work on haiku, building and reviews on sonnet, architecture, security, the data
  model, ADRs and design decisions on opus. The skills run on sonnet and delegate by agent name without passing a model (a model passed per call overrides the one in the
  file). The values are in one `models.conf` per kit and `models apply` copies them into the agents, so changing a model is editing one line.
- Why: it is enforced by the agent file, it needs no process to run, and an agent that meets a decision that is not its own is told to stop and say so, so a cheap
  model does not settle an expensive question by guessing. A skill cannot change the main session's model for good (that is `/model`), so the cheap way is a session
  on a cheaper model that delegates up.
- Considered: an MCP server (a process to keep running to expose what a command and a skill already give; against keeping the system light); a rule in CLAUDE.md or
  in the skill asking the model to remember to pick the model (not enforced); one model for everything (pays the top price for typing, or risks the cheap one deciding
  an architecture); `CLAUDE_CODE_SUBAGENT_MODEL` (one value for all agents; it is only the default and is overridden by the agent file); a plugin bundle (possible, but the
  agents are user-level files linked by `install.sh`, like the skills, with nothing to publish).
- Not measured: the mapping is a reasoned default. If a cheap agent keeps failing at some kind of task, move it up a level in `models.conf`.
- Based on the Claude Code documentation fetched on 2026-10-17 (summaries of the sub-agents, skills and plugins pages): the plugin side (that plugin agents may set a model) was
  inferred, not stated; the agents here are user-level files, so it does not matter.

## 2026-10-17: Claude Desktop installed lean (no Cowork, no keyring)
- Decided: install `claude-desktop` from Anthropic's apt repository (key fingerprint checked) with `--no-install-recommends`, plus
  `socat`. That is 2 packages; the default install pulls 55 (QEMU, OVMF, virtiofsd for the Cowork tab, plus gnome-keyring).
- Considered: full install (heavy, and Cowork needs a VM and the `kvm` group; against keeping the system light); adding
  gnome-keyring (10 more packages, and the webapp profiles use `--password-store=basic` so that no keyring ever asks for a
  password). Consequence: the app may not remember the sign-in between launches. If that is annoying, `sudo apt install
  gnome-keyring`; for Cowork, `sudo apt install qemu-system-x86 ovmf virtiofsd` and `sudo usermod -aG kvm $USER` (the CPU has svm).
- The unofficial community builds were not considered: Anthropic ships an official package for Ubuntu.

## 2026-10-17 (later): QEMU/KVM installed after all, for Cowork and the CI virtual machines
- Reversed in part the entry above: QEMU (`qemu-system-x86`, `ovmf`, `virtiofsd`; 24 packages with `--no-install-recommends`) is
  installed, and `zeke` is in the `kvm` group. Reason: Cowork is wanted, and the CI runner already decided on 2026-10-14 needs a VM on
  this desktop anyway, so the virtualization stack is shared. gnome-keyring is still left out.
- Note on 2026-10-14: the host of that VM is now Ubuntu 26.04, not Fedora. Cowork and a CI VM each reserve RAM: the desktop has 14 GiB
  today and 32 GiB is planned.
- Not decided yet: libvirt/virt-manager or plain QEMU scripts for the CI VM (separate step, options first).

## 2026-10-18: the router may run light services; runners and untrusted code still may not
Decided: `hs-gateway` can host light services and containers as long as it has spare CPU and
RAM (first candidate: none yet; the code index of ia_general_workflow_improvement runs on the
desktop because it needs the code on disk). Each one with CPU and memory limits and no host
networking. Still off limits: CI runners, build jobs and anything that runs untrusted code
(they "can break everything" on the machine every packet crosses), and changes to its firewall
or to the existing Docker setup. Files in the home directory on request, as decided on 2026-10-16.
Considered: leaving the 2026-10-14 rule as it was (it kept the router untouched, but the machine
has idle capacity and I accept the risk for small services); allowing anything (rejected: a heavy
job or a PR's code on the router would starve or expose the whole house, the reasons of
2026-10-14). This loosens the "no services or containers" part of the 2026-10-14 and 2026-10-16
entries; the rest stands.

## 2026-10-18: the Ryzen 9 desktop's root volume grows to 300 GB, not to the whole disk
- Decided (this machine only; the ThinkPad and the Arch desktop have their own disks and state files): extend `ubuntu-vg/ubuntu-lv` from 100 GB to 300 GB with `lvextend -r -L +200G` (online,
  ext4). The disk is 931 GB; the installer had allocated 100 GB and left 828 GB unassigned, so `/`
  showed 86% used with 80 GB in use. Largest users: the CI VM disk (34 GB of its 80 GB limit),
  Claude Desktop's VM bundle (12 GB), Podman images and the Rust build directory.
- Considered: extending to the whole disk (nothing is gained today and it removes the free
  space for another volume or LVM snapshots; an ext4 volume is easy to grow and hard to shrink);
  extending by 300 GB (more than the expected need of about 150 to 190 GB: current use, the CI VM
  growing to its limit, the models and indexes of the projects to come); only cleaning up (about
  6 GB, and the CI VM can still grow into what is left).
- Later: grow again the same way if needed; the unassigned space is the reserve.

