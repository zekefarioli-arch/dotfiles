# Claude Code sessions

How I start and continue Claude Code on this laptop. Always from a terminal
in ~ (claude-new changes to ~ by itself; `claude -c` and `claude -r` look for
the sessions of the folder they run in, so `cd ~` first).

| What | Command |
|---|---|
| First session, or a new one | `claude-new` |
| Continue the last session | `cd ~ && claude -c` |
| Continue a specific session | `cd ~ && claude -r fedora_desktop001` (opens the picker filtered by that name; Enter) |
| Pick from all sessions | `cd ~ && claude -r` |
| Leave a session | `/exit` or Ctrl+D (it is saved; continue it later) |

`claude-new` names sessions fedora_desktop001, fedora_desktop002, and so on;
the last number is in ~/.local/state/claude-new/last.

When to start a new one: when a session has gone on for a long time, feels
slow or starts forgetting things, or when I move to an unrelated task. Before
leaving the old one, ask Claude to update docs/STATE.md, CHANGELOG.md and
DECISIONS.md and to commit and push, so the new session starts from the
current state. ~/CLAUDE.md and Claude's memory carry over automatically.
