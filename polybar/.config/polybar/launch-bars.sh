#!/usr/bin/env bash
# Lanza una polybar por pantalla de xmonad. xmonad lo ejecuta al iniciar y
# cada vez que se conecta o desconecta un monitor: siempre parte de cero,
# así nunca quedan barras de monitores que ya no están.

# Un solo lanzamiento a la vez (varios eventos de monitor seguidos)
exec 9>"${XDG_RUNTIME_DIR:-/tmp}/polybar-launch.lock"
flock 9

killall -q polybar
pkill -u "$UID" -f '^xprop -spy -root _XMONAD_LOG_'
while pgrep -u "$UID" -x polybar >/dev/null; do sleep 0.1; done

monitors=$(polybar --list-monitors)

# Pantallas de xmonad (Xinerama, mismo orden): "N WxH+X+Y"
heads=$(xdpyinfo -ext XINERAMA 2>/dev/null |
  awk '/head #[0-9]+:/ {gsub(/[#:]/, "", $2); split($5, p, ","); print $2, $3 "+" p[1] "+" p[2]}')

seen=" "
while read -r n geom; do
  [[ -z $geom || $seen == *" $geom "* ]] && continue  # monitores espejados: una barra
  seen+="$geom "
  mon=$(awk -F': ' -v g="$geom" 'index($2, g) == 1 {print $1; exit}' <<<"$monitors")
  [[ -z $mon ]] && continue
  # La pantalla 0 lleva la barra con systray
  if [[ $n == 0 ]]; then bar=main; else bar=secondary; fi
  MONITOR=$mon SCREEN=$n polybar "$bar" 9>&- >/dev/null 2>&1 &
done <<<"$heads"

# Sin Xinerama: una barra en el primer monitor
if [[ -z $heads ]]; then
  MONITOR=${monitors%%:*} SCREEN=0 polybar main 9>&- >/dev/null 2>&1 &
fi
