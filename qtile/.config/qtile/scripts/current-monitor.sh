#!/usr/bin/env bash

# 1) outputs conectados
mapfile -t outs < <(xrandr --query | awk '/ connected/{print $1}')

# 2) primary
PRIMARY="$(xrandr --query | awk '/ connected primary/{print $1; exit}')"

# 3) arma labels y calcula maxlen (en "nombre corto", con * si es primary)
maxlen=0
declare -A label

for o in "${outs[@]}"; do
  short="${o//-/}"              # quita '-'
  if [ "$o" = "$PRIMARY" ]; then
    l="${short}*"
  else
    l="$short"
  fi
  label["$o"]="$l"
  ((${#l} > maxlen)) && maxlen=${#l}
done

# 4) detecta output actual por posición del mouse (X11) usando xrandr --listmonitors
eval "$(xdotool getmouselocation --shell)"
x="$X"; y="$Y"

current="$(
  xrandr --listmonitors | awk -v x="$x" -v y="$y" '
    NR>1 {
      name=$4
      geom=$3
      match(geom,/([0-9]+)\/[^x]+x([0-9]+)\/[^+]+\+([0-9]+)\+([0-9]+)/,m)
      w=m[1]; h=m[2]; ox=m[3]; oy=m[4]
      if (x>=ox && x<ox+w && y>=oy && y<oy+h) { print name; exit }
    }
  '
)"

# 5) imprime padded a maxlen (espacios a la derecha)
printf "%-*s\n" "$maxlen" "${label[$current]}"
