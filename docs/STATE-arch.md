# System state: Arch desktop

The current state of the Arch desktop: what is installed and configured, what is
pending and what is known to be broken. Claude Code reads this at the start of
every session on that machine, through the symlink ~/.claude/STATE.local.md; keep
it short and true, and update it in the same commit as the change it describes.
History goes in CHANGELOG.md, reasons in DECISIONS.md. The laptop's file is
STATE.md; many things (themes, xmonad, polybar) are shared and described there.

_Last updated: 2026-10-08_

## Machine
- Desktop PC: Intel Core i5-4440, 16 GB RAM (zram swap 7.7 GB), 500 GB SATA SSD,
  Intel iGPU plus NVIDIA GeForce GT 710.
- Arch Linux (rolling), kernel 7.2.x, hostname `archlinux`; fully updated on 2026-10-08.
- Two 1920x1080 monitors: HDMI-0 on the left, VGA-0 on the right (primary). The layout
  comes from the autorandr profile `2monitors`.
- User `zeke`. Login by SDDM with the Catppuccin Mocha pink theme, shown on VGA-0 only
  (`system/arch/sddm-setup.sh`, config in /etc/sddm.conf.d/10-catppuccin.conf).
- Addresses and how the machine connects to the rest of the home network are in the
  private repo home-infra, not here (this repo is public).

## Differences from the laptop (read before assuming)
- Code projects live in ~/Projects here, not ~/Zeke_projects (this overrides
  ~/CLAUDE.md). `~/.config/claude-tools/projects_dir` and `repos` already point there.
- Desktop: no battery (battery-status prints nothing, so the bar hides it) and no
  brightness keys.
- Package manager: pacman, and `paru`/`yay` for the AUR. Never do a partial upgrade:
  always `sudo pacman -Syu`, not `-Sy` alone.
- sudo asks for a password; there is no askpass helper. Root steps are run by me in a
  terminal (`ssh -t` from the laptop works); Claude cannot do them alone.
- Claude Code is reached from the laptop over ssh with a dedicated key.
- The dotfiles checkout is ~/dotfiles on branch `main`. The branch `arch-local` (only in
  the home git hub) keeps the old, simpler Arch configuration as a backup.

## Desktop
- xmonad, Polybar, rofi, dunst, GTK and Qt themes, tmux, starship, micro and alacritty
  come from the shared dotfiles, installed with
  `stow --no-folding -t ~ xmonad polybar bin alacritty tmux rofi dunst gtk qt x11 xdg micro picom starship`.
  Not stowed on purpose: nvim, bash (its own .bashrc: starship prompt, mise, a
  `claude-omni` helper), copyq, webapps, qtile, wezterm and terminator.
- Hot-plug of monitors: the shared xmonad.hs runs `autorandr --change` and relaunches the
  bars when a monitor is plugged or unplugged (tested only by the laptop's simulation).
- Terminal: alacritty (Super+Enter) and the dropdown on Super+Ctrl+T (tmux session `drop`).
  wezterm is still installed.
- picom starts from autostart.sh unless ~/.config/picom/disabled exists (it does not).
  The shared config blurs only the dropdown terminal; its slide animation was made slower
  on 2026-10-08 after it looked rough here.
- Screenshots: Print (area) saves to ~/Pictures/Screenshots and copies to the clipboard.
- Wallpaper: ~/.fehbg, one image per monitor (autostart.sh uses it when the Catppuccin
  wallpaper of the laptop is not there).
- Browsers: Brave is the default (mimeapps.list); Firefox, Chromium and others are also
  installed. Keep the number of open tabs low: with 60+ tabs memory ran out on 2026-10-08.
- Terminal prompt: starship with the laptop's Catppuccin powerline config; ble.sh was
  removed.
- Apps worth knowing: Docker (running, enabled at boot), CUPS, Antigravity, Claude
  Desktop, scrcpy and android-tools, Papirus icons, Catppuccin cursor in
  ~/.local/share/icons.

## Claude Code
- claude-tools and webapps are cloned in ~/Projects and linked into ~/.local/bin by their
  install.sh; the machine name is `arch`, so sessions are named `<path>_NNN@arch`.
- Super+a opens claude-pick (shared xmonad.hs).
- ~/CLAUDE.md is a symlink to ~/dotfiles/claude/CLAUDE.md, and ~/.claude/STATE.local.md
  is this file.

## Pending
- Try on screen: hot-plug of a monitor, the login with one monitor, the Print shortcut,
  and every shortcut in the shared keys (Super+F1 lists them).
- Stow the remaining packages if wanted: webapps (then `webapp sync`), copyq, nvim.
- ssh hardening (keys first, then passwords off and a rule for the LAN only); the plan is
  in home-infra.
- Disk: the root partition is 89% full (45 GB); clean the pacman cache and old Docker
  images before the next big update.
- Sharing Claude sessions between the two machines: not decided (see STATE.md).

## Known issues
- The NVIDIA GT 710 needs the legacy driver `nvidia-470xx-dkms` (AUR). After a kernel
  update check that the module builds (`dkms status`) before rebooting, or X will not
  start; a TTY and ssh still work.
- Memory gets tight with many browser tabs and several Claude agents at once (load
  reached 150 on 2026-10-08). Do not start several agents in parallel here.
