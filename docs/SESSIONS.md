# Claude Code sessions

The tools that start, continue and cut Claude Code sessions moved to their own
repository, so they work on any computer (here and on the Arch desktop) without
the rest of these dotfiles:

- https://github.com/zekefarioli-arch/claude-tools, cloned in
  ~/Zeke_projects/claude-tools. Its README explains `claude-pick` (Super+a),
  `claude-new` and `claude-fresh`; docs/SESSIONS.md there is the daily guide.

What stays here: the `Super+a` action (xmonad's actions.conf and keys.conf), the
list of extra repositories that `claude-fresh` updates when Claude is opened in ~
(`claude/.config/claude-tools/repos`), ~/CLAUDE.md and the state docs
(docs/STATE.md for the Fedora laptop; the desktop has its own).

Per-machine setup (once, after cloning the dotfiles on a computer): link its own
state file, which ~/CLAUDE.md imports, and name the machine.

    ln -s ~/dotfiles/docs/STATE.md ~/.claude/STATE.local.md   # Fedora laptop
    ./install.sh --name arch                                  # in claude-tools

On a new machine write its own docs/STATE-<name>.md (same layout as STATE.md)
and link that instead.
