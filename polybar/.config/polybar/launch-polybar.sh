#!/usr/bin/env bash

# Detect connected monitors using xrandr
connected_monitors=($(xrandr --query | grep " connected" | cut -d" " -f1))

# Kill any running polybar instances
killall -q polybar

# Print the number of connected monitors and their names
echo "Detected $((${#connected_monitors[@]})) monitor(s):"
for monitor in "${connected_monitors[@]}"; do
    echo "- $monitor"

    # Determine which bar to launch based on the number of monitors
    case ${#connected_monitors[@]} in
        1)
            MONITOR=$monitor polybar main &
            ;;
        2)
            if [ "$monitor" == "${connected_monitors[0]}" ]; then
                MONITOR=$monitor polybar main &
            else
                MONITOR=$monitor polybar secondary &
            fi
            ;;
        *)
            if [ "$monitor" == "${connected_monitors[0]}" ]; then
                MONITOR=$monitor polybar main &
            elif [ "$monitor" == "${connected_monitors[1]}" ]; then
                MONITOR=$monitor polybar secondary &
            else
                MONITOR=$monitor polybar tertiary &
            fi
            ;;
    esac
done

# Wait for all background jobs to complete
wait
