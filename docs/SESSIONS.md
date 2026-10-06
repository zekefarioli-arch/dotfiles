# Claude Code sessions

How I start and continue Claude Code on this laptop. Always from a terminal
in the folder you work on (`claude -c` and `claude -r` look for the sessions
of the folder they run in, so `cd` there first).

| What | Command |
|---|---|
| First session in a folder, or a new one | `claude-new` (in the folder, or `claude-new myproj` for ~/Zeke_projects/myproj) |
| Continue the last session | `cd ~ && claude -c` |
| Continue a specific session | `cd ~ && claude -r d36e_home_001` (opens the picker filtered by that name; Enter) |
| Pick from all sessions | `cd ~ && claude -r` |
| Leave a session | `/exit` or Ctrl+D (it is saved; continue it later) |

`claude-new` names sessions <machine>_<folder>_NNN: d36e_myproj_001,
d36e_myproj_002, and so on (folder is `home` in ~). The machine part is the
first 4 characters of /etc/machine-id, so two computers never share a name; put
a readable one in ~/.config/claude-new/machine (for example `arch`) to
replace it. A folder with no named session starts _001; if
it has some, the last one is shown and Enter continues it while `n` starts the
next number. The numbers are read from the sessions Claude saved for that
folder, so there is no counter file. Sessions are stored per machine and are not
shared; the docs, memory notes and projects
travel through GitHub.

Per-machine setup (once, after cloning dotfiles on a computer): link its own
state file, which ~/CLAUDE.md imports, and optionally name the machine.

    ln -s ~/dotfiles/docs/STATE.md ~/.claude/STATE.local.md   # Fedora laptop
    echo arch > ~/.config/claude-new/machine                  # optional name

On a new machine write its own docs/STATE-<name>.md (same layout as STATE.md)
and link that instead. Printer addresses go in ~/.config/printers.env.

When to start a new one: when a session has gone on for a long time, feels
slow or starts forgetting things, or when I move to an unrelated task. Before
leaving the old one, ask Claude to update docs/STATE.md, CHANGELOG.md and
DECISIONS.md and to commit and push, so the new session starts from the
current state. ~/CLAUDE.md and Claude's memory carry over automatically.
