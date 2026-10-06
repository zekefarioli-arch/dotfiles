# System state

The current state of the laptop: what is installed and configured, what is
pending and what is known to be broken. Claude Code reads this at the start of
every session; keep it short and true, and update it in the same commit as
the change it describes. History goes in CHANGELOG.md, reasons in
DECISIONS.md.

_Last updated: 2026-10-06_

## Machine
- Lenovo ThinkPad X13 Gen 1 AMD (20UGS2Q500), 30 GiB RAM, 236 GB NVMe.
- BIOS R1CET79W (1.48).
- Fedora 44, kernel 7.2.x, btrfs root with snapper snapshots.
- Wi-Fi only (192.168.0.0/24 at home); timezone Europe/London, set
  automatically by a NetworkManager dispatcher script.
- User `zeke`; LightDM starts the xmonad session.

## Desktop
- xmonad: action catalog in xmonad.hs, shortcuts in ~/.xmonad/keys.conf,
  custom actions in ~/.xmonad/actions.conf. `keys-editor` edits them and
  `keybinds` (Super+F1 or Super+/) lists them.
- Polybar: one bar per monitor, launched by xmonad (launch-bars.sh). Modules:
  workspaces, window title, Wi-Fi, CPU, memory, battery script, date.
- picom (glx, vsync): blur and slide animation for the dropdown terminal.
- Dropdown terminal: alacritty (class dropterm) with tmux session `drop`,
  toggled by Super+Ctrl+T; it hides before rofi, Super+Enter and Super+E.
- Lock: i3lock through `lock-screen`; Super+L and xss-lock (suspend, lid).
- Theme: Catppuccin Mocha with a pink accent in GTK, Qt, rofi, dunst, CopyQ,
  alacritty, tmux, Neovim, Polybar; Papirus-Dark icons.
- Apps: Brave (default browser), Thunar, Atril, Neovim (LazyVim),
  Mousepad (default for text files, Catppuccin Mocha colours), CopyQ,
  KDE Connect and scrcpy for the phone, simple-scan.

## Calendar
- Click on the bar's date: `mini-calendar` (GTK) with Google Calendar events
  from secret iCal addresses in ~/.config/mini-calendar/ics-url (private,
  mode 600, never in the repo) and an "Ask Claude…" box (headless Claude Code
  with the Google Calendar connector).
- Super+Shift+C: Google Calendar in its own fullscreen Brave profile
  (~/.local/share/calendar-app) with the Claude extension (Ctrl+E).
  Super+Ctrl+C: that profile's extensions and theme.

## Printing and scanning
- CUPS with two queues; cups-browsed is disabled.
  - Brother_MFC_L2700DN (default): driverless IPP at 192.168.0.11, two-sided.
  - Canon_iX6800: through the CUPS server hs-media-srv (192.168.0.137), USB.
- Scanner: Brother MFC-L2700DN through sane-airscan (WSD), pinned in
  /etc/sane.d/airscan.conf. Setup: scripts/setup-printers.sh.

## Claude Code
- Sessions run from ~ and start with `claude-new` (fedora_desktopNNN);
  how to start and continue them: docs/SESSIONS.md.
- ~/CLAUDE.md (dotfiles package `claude`) holds the standing instructions;
  Claude's own memory is in ~/.claude/projects/-home-zeke/memory/.

## Pending
- Firmware: BIOS 1.54 and Secure Boot KEK/UEFI CA 2023 plus dbx updates are
  available. Needs the charger plugged in: `sudo fwupdmgr update`.
- Passwordless sudo (/etc/sudoers.d/zeke-nopasswd) was added for the setup;
  remove it when the setup is done.
- Calendar: creating and editing events from the mini calendar itself
  (OAuth) is deferred; for now the Ask Claude box does it.
- Polybar hot-plugging was only tested with simulated monitors; confirm with
  a real external monitor.

## Known issues
- The dropdown terminal does not hide by itself when it loses the focus
  (nsHideOnFocusLoss has no effect); it hides on the shortcuts listed above.
- The local DNS (router) fails to resolve some hosts, such as
  download.brother.com; resolve through another DNS if a download fails.
