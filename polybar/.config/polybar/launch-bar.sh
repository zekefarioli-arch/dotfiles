#!/usr/bin/env bash
# Lanza la polybar de la pantalla N de xmonad. La llama xmonad (una por
# monitor) al iniciar y cada vez que se conecta o desconecta un monitor.
N="${1:-0}"

# Pantalla N de xmonad (Xinerama) -> nombre del monitor en polybar, por geometría
geom=$(xdpyinfo -ext XINERAMA 2>/dev/null |
  awk -v h="head #$N:" 'index($0, h) {split($5, p, ","); print $3 "+" p[1] "+" p[2]; exit}')
MONITOR=$(polybar --list-monitors | awk -F': ' -v g="$geom" 'g != "" && index($2, g) == 1 {print $1; exit}')
[[ -z $MONITOR ]] && MONITOR=$(polybar --list-monitors | sed -n "$((N + 1))s/:.*//p")

# El monitor 0 lleva la barra con systray
if [[ $N == 0 ]]; then BAR=main; else BAR=secondary; fi

export MONITOR SCREEN="$N"
exec polybar "$BAR"
