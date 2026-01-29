#!/usr/bin/env bash
# Uso: ./xmonad-windows.sh 0 o 1
SID="${1:-0}"

MAX_CHARS=45   # limite total SOLO para el texto (no incluye el icono)
ELLIPSIS="..." # podes cambiar a "…" si queres

# Definición de iconos y colores
ICO_DEFAULT="%{F#6c7086}󰖲%{F-}"        # Icono por defecto (Gris)
ICO_VSCODE="%{F#89b4fa}%{F-}"         # VSCode (Azul)
ICO_KITTY="%{F#f5e0dc}%{F-}"          # Kitty (Flamingo)
ICO_THUNAR="%{F#f9e2af}󰷏%{F-}"         # Thunar (Amarillo)
ICO_BRAVE="%{F#fab387}%{F-}"          # Brave (Naranja)

truncate_text() {
  local s="$1"
  local max="$2"
  local ell="$3"

  if [ "${#s}" -le "$max" ]; then
    printf '%s' "$s"
    return
  fi

  local keep=$(( max - ${#ell} ))
  if [ "$keep" -lt 0 ]; then
    keep=0
  fi

  printf '%s%s' "${s:0:$keep}" "$ell"
}

dbus-monitor --session "type='signal',interface='org.xmonad.Log',member='Update'" |
while read -r line; do
  if [[ "$line" == *"string"* ]]; then
    payload=$(echo "$line" | sed -n 's/.*string "\(.*\)".*/\1/p')
    raw_title=$(echo "$payload" | sed -n "s/.*WIN$SID:\([^WLS]*\).*/\1/p")
    clean_text=$(echo "$raw_title" | sed 's/%{[^}]*}//g' | xargs)

    # 1. Procesar redundancia y obtener la aplicación
    if [[ "$clean_text" == *" - "* ]]; then
      app_lower=$(echo "$clean_text" | cut -d'-' -f1 | xargs | tr '[:upper:]' '[:lower:]')
      final_output=$(echo "$clean_text" | awk -F' - ' '{
        out = $1;
        for (i=2; i<=NF && i<=3; i++) {
          if ($i != $1 && $i != $(i-1)) { out = out " - " $i; }
        }
        print out;
      }')
    else
      app_lower=$(echo "$clean_text" | tr '[:upper:]' '[:lower:]')
      final_output="$clean_text"
    fi

    # 2. Asignar icono según la aplicación
    case "$app_lower" in
      *code*)   ICON="$ICO_VSCODE" ;;
      *kitty*)  ICON="$ICO_KITTY"  ;;
      *thunar*) ICON="$ICO_THUNAR" ;;
      *brave*)  ICON="$ICO_BRAVE"  ;;
      *)        ICON="$ICO_DEFAULT" ;;
    esac

    # 3. Salida final
    if [[ -z "$final_output" || "$final_output" == "-" ]]; then
      echo "$ICO_DEFAULT Desktop"
    else
      final_output="$(truncate_text "$final_output" "$MAX_CHARS" "$ELLIPSIS")"
      echo "$ICON $final_output"
    fi
  fi
done
