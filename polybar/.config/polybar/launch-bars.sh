#!/usr/bin/env bash
# Start one polybar per xmonad screen. xmonad runs it at startup and every
# time a monitor is connected or disconnected: it always starts from scratch,
# so no bars are left behind for monitors that are gone.

# One launch at a time (bursts of monitor events)
exec 9>"${XDG_RUNTIME_DIR:-/tmp}/polybar-launch.lock"
flock 9

killall -q polybar
pkill -u "$UID" -f '^xprop -spy -root _XMONAD_LOG_'
while pgrep -u "$UID" -x polybar >/dev/null; do sleep 0.1; done

monitors=$(polybar --list-monitors)

# xmonad screens (Xinerama, same order): "N WxH+X+Y"
heads=$(xdpyinfo -ext XINERAMA 2>/dev/null |
  awk '/head #[0-9]+:/ {gsub(/[#:]/, "", $2); split($5, p, ","); print $2, $3 "+" p[1] "+" p[2]}')

seen=" "
while read -r n geom; do
  [[ -z $geom || $seen == *" $geom "* ]] && continue  # mirrored monitors: one bar
  seen+="$geom "
  mon=$(awk -F': ' -v g="$geom" 'index($2, g) == 1 {print $1; exit}' <<<"$monitors")
  [[ -z $mon ]] && continue
  # Screen 0 gets the bar with the systray
  if [[ $n == 0 ]]; then bar=main; else bar=secondary; fi
  MONITOR=$mon SCREEN=$n polybar "$bar" 9>&- >/dev/null 2>&1 &
done <<<"$heads"

# No Xinerama: one bar on the first monitor
if [[ -z $heads ]]; then
  MONITOR=${monitors%%:*} SCREEN=0 polybar main 9>&- >/dev/null 2>&1 &
fi
