# Zeke's computers

Two computers share this account and the dotfiles repo: a ThinkPad X13 Gen 1
(AMD) laptop with minimal Fedora 44, and an Arch desktop. Both use xmonad,
Polybar, picom and Catppuccin Mocha with a pink accent. Sessions run from ~
or from a project folder and are started with `claude-new`, which names them
<machine>_<folder>_NNN (folder is `home` in ~).

## Read first: the system state of this machine
@~/.claude/STATE.local.md

Each machine has its own state file in ~/dotfiles/docs/ (STATE.md is the
Fedora laptop's) and a local symlink ~/.claude/STATE.local.md pointing to
it, which is imported above so it is in every session. If that import is
empty, this machine has no link yet: say so and offer to create it
(`ln -s ~/dotfiles/docs/<its file> ~/.claude/STATE.local.md`, and start the
file from STATE.md's layout). Trust the system over the file: if something
you check disagrees with it, say so and fix the file.

## Keep the docs current
In the same commit as each change, update what it touches:
- The state file of the machine you are on (`docs/STATE.md` for the Fedora
  laptop): what is installed, configured, pending or broken now
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
- The Claude session tools (`claude-new`, `claude-pick`, `claude-fresh`) and
  `webapp` are not here: they are the repos claude-tools and webapps in
  ~/Zeke_projects, linked into ~/.local/bin by their install.sh. Change them
  there, test them there and push there; only my list of apps and the Super+a
  key live in these dotfiles.
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
