#!/usr/bin/env bash
# Show in polybar what xmonad publishes for screen N (_XMONAD_LOG_N):
# active monitor, workspaces, layout and focused window.
N="${1:-0}"
while true; do
  xprop -spy -root "_XMONAD_LOG_$N" 2>/dev/null |
    sed -u -n 's/^[^"]*"\(.*\)"$/\1/p' | sed -u 's/\\"/"/g; s/\\\\/\\/g'
  sleep 1  # the property does not exist yet at startup
done
