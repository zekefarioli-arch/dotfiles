# Zeke's Fedora laptop

ThinkPad X13 Gen 1 (AMD), minimal Fedora 44 with xmonad, Polybar, picom and
Catppuccin Mocha with a pink accent everywhere. Sessions run from ~ and are
started with `claude-new` (named fedora_desktopNNN).

## Read first: the system state
@~/dotfiles/docs/STATE.md

That file is imported above so it is in every session. Trust the system over
it: if something you check disagrees with it, say so and fix the file.

## Keep the docs current
In the same commit as each change, update what it touches:
- `docs/STATE.md`: what is installed, configured, pending or broken now
  (and its "Last updated" date).
- `docs/CHANGELOG.md`: one line under today's date for changes that matter
  to the user.
- `docs/DECISIONS.md`: when we choose between alternatives that shape the
  system, an entry with what was decided, what else was considered and why.
  Read it before proposing to undo or redo something; if a decision is
  reversed, add a new entry instead of deleting the old one.
- In code and configs, a short comment next to a non-obvious choice saying
  why (and what was rejected, if it matters), like the existing scripts do.
- README.md when the behaviour or the shortcuts change.

## How I work
- Chat in Spanish; everything written to files (code, configs, comments,
  READMEs, commit messages) in English.
- Keep the system light: prefer small single-purpose tools, check what a
  package pulls in before installing, avoid Flatpak runtimes unless needed.
- Code projects live in ~/Zeke_projects (GitHub: zekefarioli-arch).

## Dotfiles
- ~/dotfiles (github.com/zekefarioli-arch/dotfiles, branch main) holds every
  config as GNU Stow packages: `stow --no-folding -t ~ <package>`.
- Change the files in ~/dotfiles, not the symlinks' targets elsewhere; files
  outside $HOME go in `system/` with install notes in their header.
- When I say "comitealo y pushealo": run the tests
  (`python3 -m unittest discover -s tests`), commit with a clear English
  message, push to main.
- The README documents each part; update it when behaviour changes.

## xmonad
- Shortcuts live in ~/.xmonad/keys.conf (apply with `xmonad --restart`),
  custom actions in ~/.xmonad/actions.conf; the catalog is in xmonad.hs
  (recompile with `xmonad --recompile`). `keys-editor` and `keybinds` manage
  and list them; `keys-editor` with no arguments opens rofi, so use its CLI
  (list, set, remove, reset, free, new) from scripts.

## Safety in my live session
- Never simulate keyboard or mouse input (xdotool key/type/click) in my X
  session: it once locked me out. Test logic headlessly and ask me to try
  the UI.
- Never `pkill -f` a pattern that appears in your own command; kill by exact
  name or PID.
- Never print or commit secrets, such as the Google Calendar iCal address in
  ~/.config/mini-calendar/ics-url.
- If I reject or interrupt a command, check what actually changed before
  doing anything else.
