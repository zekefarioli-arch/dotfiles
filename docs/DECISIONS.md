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
  WebPanel-* window upwards by ~86 px when it is at the top edge of its area, so
  the tab strip and toolbar fall off the screen. The window stays tiled.
- Why: the Claude side panel needs a normal window, which always shows Brave's
  toolbar; it has to be hidden from outside.
- Considered: --app (no side panel); Brave fullscreen (hides the UI but covers
  the bar and other windows, and a hook to sink it again fought two EWMH
  handlers: tried and replaced, commit b70672e); floating window offset
  upwards (leaves the layout); a Chromium patch (too heavy).
- Google Calendar follows the same rule (class WebPanel-calendar, no
  fullscreen) since its fullscreen did not fit xmonad's Full layout.
- Limits: height is a constant to tune; a web app below another window in a
  column keeps its toolbar visible.
