#!/usr/bin/env bash
set -euo pipefail

BAR="${1:-}"

if [[ -z "$BAR" ]]; then
  echo "Usage: $0 <bar-name>  (e.g. main | second)" >&2
  exit 2
fi

PID="$(pgrep -n -f "polybar ${BAR}" || true)"

if [[ -z "$PID" ]]; then
  echo "No running polybar process found for: polybar ${BAR}" >&2
  exit 1
fi

polybar-msg -p "$PID" cmd toggle
