# Antigravity CLI (agy): ask which model to use when a conversation starts, and keep it for that conversation.
# Why: agy has no default-model setting (its settings.json has no model key and `/model` changes one prompt
# only), and its default, Gemini 4 Argon, has a separate weekly quota that ran out on 2026-10-26 without
# anyone choosing it. `--model` at launch fixes the model for the whole session, so the function asks first.
# Enter takes the default (Gemini 3.8 Flash Medium), `q` cancels. A function, not an alias, so the
# subcommands (`agy models`, `agy update`...) do not get a --model they would reject.
# No question (the default model, or the one you name, is used straight away) when:
#   --model is given            agy --model gemini-4-argon-high
#   AGY_MODEL is set            AGY_MODEL=claude-sonnet-5-5-medium agy
#   it is not a person typing   -p/--print/--prompt, or stdin or stdout is not a terminal (scripts, pipes)
# Scripts and the `gemini` skill call the real binary and pick their model themselves. `command agy` skips all this.

_agy_pick_model() {
    local default=gemini-3.8-flash-medium cache="${XDG_CACHE_HOME:-$HOME/.cache}/agy-models.tsv"
    # The list comes from `agy models` (needs the network); keep it a day and keep the old one if a refresh fails.
    if [ ! -s "$cache" ] || [ -n "$(find "$cache" -mmin +1440 2>/dev/null)" ]; then
        mkdir -p "${cache%/*}"
        command agy models 2>/dev/null | awk -F'\t' 'NF >= 2' > "$cache.tmp"
        [ -s "$cache.tmp" ] && mv "$cache.tmp" "$cache"
        rm -f "$cache.tmp"
    fi
    local -a ids=() names=()
    local id name i d=0 n
    while IFS=$'\t' read -r id name; do ids+=("$id"); names+=("$name"); done < "$cache" 2>/dev/null
    if [ "${#ids[@]}" -eq 0 ]; then echo "$default"; return; fi   # no list at all: do not block the user
    for i in "${!ids[@]}"; do
        [ "${ids[i]}" = "$default" ] && d=$((i + 1))
        printf ' %s %2d) %s\n' "$([ "${ids[i]}" = "$default" ] && echo '*' || echo ' ')" "$((i + 1))" "${names[i]}" >&2
    done
    while true; do
        read -r -p "Model [${d:-1}] (q = cancel): " n >&2 || return 1
        n=${n:-${d:-1}}
        [ "$n" = q ] && return 1
        case "$n" in
            ''|*[!0-9]*) ;;
            *) if [ "$n" -ge 1 ] && [ "$n" -le "${#ids[@]}" ]; then echo "${ids[n-1]}"; return; fi ;;
        esac
        echo "Pick a number from 1 to ${#ids[@]}, or q." >&2
    done
}

agy() {
    local arg headless=0 model="${AGY_MODEL:-}"
    for arg in "$@"; do
        case "$arg" in
            --model|--model=*) command agy "$@"; return ;;
            -p|--print|--prompt) headless=1 ;;
        esac
    done
    case "${1:-}" in
        agent|agents|changelog|help|install|mcp|mic-serve|models|plugin|plugins|remote-control|update)
            command agy "$@"; return ;;
    esac
    if [ -z "$model" ]; then
        if [ "$headless" = 0 ] && [ -t 0 ] && [ -t 1 ]; then
            model=$(_agy_pick_model) || return 130
        else
            model=gemini-3.8-flash-medium
        fi
    fi
    command agy --model "$model" "$@"
}
