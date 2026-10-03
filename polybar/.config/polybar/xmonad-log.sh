#!/usr/bin/env bash
# Muestra en polybar lo que xmonad publica para la pantalla N (_XMONAD_LOG_N):
# monitor activo, workspaces, layout y ventana con foco.
N="${1:-0}"
while true; do
  xprop -spy -root "_XMONAD_LOG_$N" 2>/dev/null |
    sed -u -n 's/^[^"]*"\(.*\)"$/\1/p' | sed -u 's/\\"/"/g; s/\\\\/\\/g'
  sleep 1  # la propiedad todavía no existe al arrancar
done
