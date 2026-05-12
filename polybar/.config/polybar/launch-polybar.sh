#!/usr/bin/env bash

# Detect connected monitors using xrandr
connected_monitors=($(xrandr --query | grep " connected" | cut -d" " -f1))

# Print the number of connected monitors and their names
echo "Detected $((${#connected_monitors[@]})) monitor(s):"
for monitor in "${connected_monitors[@]}"; do
    echo "- $monitor"
done
