# Polybar Configuration

## How Monitor Detection Works

The system detects connected monitors using `xrandr` and launches the appropriate number of Polybar instances based on the number of monitors detected. The script `launch-polybar.sh` handles this logic.

## What launch-polybar.sh Does

- Detects all connected monitors.
- Kills any running Polybar instances to avoid conflicts.
- Launches the correct number of Polybar instances (`main`, `secondary`, or `tertiary`) based on the number of monitors detected.
- Assigns each Polybar instance to a specific monitor.

## Configuring the Three Bars

### [bar/main]

- **tray-position = right**: Displays the system tray on the right side of the bar.
- **monitor = ${env:MONITOR}**: Automatically assigns this bar to the correct monitor based on the script's logic.

### [bar/secondary] and [bar/tertiary]

- **monitor = ${env:MONITOR}**: Automatically assigns these bars to the correct monitors based on the script's logic.
- No `tray-position` is specified, so the system tray will not be displayed on these bars.

## Keybindings in XMonad

The following keybindings control Polybar:

- **M-S-b**: Toggles all three bars (`main`, `secondary`, and `tertiary`).
- **M-b**: Toggles only the `main` bar.
- **M-C-b**: Toggles only the `secondary` bar.

## Troubleshooting

If you encounter issues with Polybar not appearing:

1. **Check Monitor Detection**:
   - Ensure that `xrandr` is correctly identifying your monitors.
   - Run `xrandr --query | grep " connected"` to see if all connected monitors are listed.

2. **Verify launch-polybar.sh Execution**:
   - Check the output of `launch-polybar.sh` when you start XMonad or restart Polybar.
   - Ensure that the script is correctly identifying the number of monitors and launching the appropriate bars.

3. **Check for Errors in Logs**:
   - Look at your system logs (e.g., using `journalctl`) to see if there are any errors related to Polybar or `xrandr`.

4. **Ensure Polybar is Installed**:
   - Make sure that Polybar is installed on your system.
   - You can install it using your package manager, e.g., `sudo pacman -S polybar` for Arch Linux.

5. **Check Permissions**:
   - Ensure that the script has the necessary permissions to run and access required resources.
   - You can set execute permissions with `chmod +x launch-polybar.sh`.

If you follow these steps and still encounter issues, please provide more details about your setup and any error messages you are seeing.
