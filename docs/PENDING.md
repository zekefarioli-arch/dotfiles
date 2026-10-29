# Pending (Ryzen 9 desktop)

Moved out of STATE.md on 2026-10-29. Read it before proposing new work, or when a task may already be listed here. Move an item back to STATE.md only when it is broken now.

- CI VM trust (2026-10-18): the user `ci` that runs the jobs has `NOPASSWD:ALL` and Docker access, so a job is root inside the VM, and the VM's NAT network reaches the home LAN. Fine for my own repositories; narrow it and cut the route to the LAN before running anyone else's pull requests.
- CI runner VM (2026-10-17): `mock-oclock` (pull request 82, ADR 046 and 047) and `due-card` (ADR 0028 in that repo) run their CI on it
  while `CI_RUNS_ON` is set. The panels are plain HTTP on the home network: not tried over the VPN. Not tried yet: a second runner per repo so
  two jobs overlap (the lock allows one at a time in the VM), and an ephemeral runner. Not tried yet: a second runner so two jobs overlap, and an ephemeral runner (needs a host-side
  controller so no GitHub token lives in the VM). Suspend: systemd's suspend.target is not masked, so it is not proven off; check before relying on the VM overnight.
- Hyprland (2026-10-17): installed system-wide; the trial is the user `zeke-hypr` (admin; pick "Hyprland" at the login screen),
  config in its `hyprland` branch (step 1: Catppuccin, xmonad shortcuts, waybar). Not tried on screen yet. Ported next, if it is kept:
  keybinds/keys-editor, dropdown terminal, web-app panels (WebAppCrop is xmonad-only), the six scripts that use xdotool.
- Brightness keys need the `video` group: log out and in once (2026-10-17) so the session has it.
- Ubuntu migration, still open (2026-10-17): ssh passwords are still on and
  /etc/sudoers.d/zeke-nopasswd (setup convenience) is still there; turn passwords off
  (`/etc/ssh/sshd_config.d/10-no-password.conf`) and remove the sudoers file when the setup is accepted.
  Antigravity IDE 1.107 (tarball 2.5.5) and the CLI `agy` 1.4.0 are installed again (2026-10-17); the IDE still needs its AppArmor profile
  (`system/etc/apparmor.d/antigravity-ide`, installed in /etc/apparmor.d on 2026-10-17, steps in its header) or Ubuntu blocks its sandbox.
  `agy` asks which model to use when you start it in a terminal (`bash/.bashrc.d/agy.sh`, 2026-10-26: Argon's own weekly quota was empty and had been spent unnoticed); Enter = Gemini 3.8 Flash Medium; `AGY_MODEL=<id> agy` or `--model` skip the question.
  Both are signed in; `agy --print "/usage" --output-format json --mode plan < /dev/null` shows the quota left.
  Not tried on screen yet: the look of the bars and picom's rules, Super+Ctrl+T, `findajob` with Alt+G,
  Super+O, sound, brightness and the lid. Keyboard layout is `gb`; say if it should be another.
  The backup on hs-gateway (~/migration-2026-10-16, holds private keys) is still to delete when you are sure.
  Delete the stray /etc/sudoers.d/zeke-nopasswd. on the Arch desktop (created by mistake).
- Ubuntu migration done on 2026-10-17 (plan in docs/UBUNTU-MIGRATION.md); the old 236 GB Fedora disk is kept as the way back. The picom 12.5 check is still to do.
- Login screen (2026-10-17): LightDM greeter with a Catppuccin wallpaper (/usr/share/backgrounds/login-kitty.jpg) and the theme and cursor copied to /usr/share; not seen on screen yet. Its popup menus are flat (2026-10-19, `system/etc/lightdm/greeter-gtk.css`, installed as the `lightdm` user's gtk.css) because the round corners showed square edges without a compositor; to check at the next login.
- Firmware: BIOS 1.54 and Secure Boot KEK/UEFI CA 2023 plus dbx updates are
  available. Needs the charger plugged in: `sudo fwupdmgr update`.
- Passwordless sudo (/etc/sudoers.d/zeke-nopasswd) was added for the setup;
  remove it when the setup is done.
- Claude sessions: the older fedora_desktop003 is not matched by `claude-new`;
  open it with `claude -r`.
- Claude sessions: no way yet to delete old ones (to do later).
- Web apps: FireShot (whole page to PDF or image) is not installed in the shared
  profile yet: `webapp extensions`, then set its shortcut in
  brave://extensions/shortcuts (suggested Alt+Shift+P for PDF, Alt+Shift+I for
  image; xmonad has no Alt shortcuts).
- Menus: the picom catch-all rule (no blur on translucent windows) and the flat
  GTK menus (gtk.css) are not confirmed on screen; open Brave's menu and
  Mousepad's File menu to check them.
- Web apps: Google Messages needs its Claude extension installed
  (`webapp extensions google-messages`) and the phone pairing again. A launcher
  "WhatsApp Web" (brave-hnpfjng...) may reappear in rofi; remove it from
  brave://apps in the WhatsApp window.
- claude-fresh: needs a real test between the two computers (push from one, open
  the project on the other). Nothing is pulled when the tree has uncommitted
  changes, by design.
- claude-pick: not tried yet: "Other folder…", resuming an old session, a
  folder with no sessions.
- Arch desktop: clone dotfiles, claude-tools and webapps (in ~/Zeke_projects),
  run each install.sh (`./install.sh --name arch` in claude-tools, `--example`
  not needed because the list comes from dotfiles), then `stow --no-folding -t ~
  webapps claude xmonad` plus the common packages, `webapp sync`, `xmonad
  --recompile`, its own docs/STATE-arch.md linked as ~/.claude/STATE.local.md,
  Brave (AUR brave-bin) and ~/.config/printers.env if it prints;
  setup-printers.sh uses dnf, so it needs a pacman version there.
- Calendar: creating and editing events from the mini calendar itself
  (OAuth) is deferred; for now the Ask Claude box does it.
- Polybar hot-plugging was only tested with simulated monitors; confirm with
  a real external monitor.

- Network info: Super+I (`show-ip`, runs `net-info`) shows a notification with the connection, the IP, the gateway and the name of this machine; a click on the bar's Wi-Fi module does the same. Added and tried on screen on 2026-10-17.
- CI runners: Super+C (`ci-status`) shows a notification with the GitHub Actions runners of the CI VM: how many are online and working, the memory of each one (its processes + file cache + peak), what the VM uses inside and what it holds on this desktop, and how many runs wait on GitHub; and, when nothing is working or waiting on GitHub and the VM holds 0.5 GiB or more than it uses, it asks the VM to drop its disk cache (`ci-trim-memory.sh --now` over ssh with sudo) and replaces the notification with how much came back (`--no-trim` only looks); the last line, in the pink accent, is the total this desktop pays (what the VM holds, with its share of RAM). It reads the VM over ssh as `ci` (address in `~/.config/ci-status/vm`, private) and `gh`; `ci-status --print` shows the same in a terminal. Added 2026-10-18; the shortcut needs `xmonad --restart` once.
- xmonad reload: Super+Q (`restart-xmonad`) runs `xmonad-reload` (added 2026-10-18; it replaces `xmonad --recompile; xmonad --restart`, which restarted even when the compilation failed). It compiles, then restarts only on success and notifies "xmonad configuration reloaded"; on an error it does not restart, copies the error to the clipboard, shows it in a floating window (class `xmonad-error`, centred by a rule in xmonad.hs) and notifies. It hides the `xmessage` window that `xmonad --recompile` opens on failure. Needs one Super+Q (the old behaviour) to take effect.
- VPN to the house: NetworkManager connection `wg-home` (WireGuard, split tunnel, no autoconnect), toggled with Super+O (`home-vpn`). Needed away from home: a phone hotspot does not carry the phone's own VPN to tethered devices. The client key lives only in NetworkManager; the server name and subnets are in home-infra.
- Network: this desktop has a fixed address (`.23`) through a DHCP reservation in the home router.
  The addresses, the machines and how they connect (router, media server, Arch desktop,
  printers, Docker services) are documented in the private repo home-infra, a private
  GitLab project cloned in ~/Zeke_projects/home_network/home-infra, and not here, because this repo is
  public. Private repos could not be created on GitHub on 2026-10-07 (HTTP 500), so that one
  lives on GitLab; the public repos (dotfiles, claude-tools, webapps) stay on GitHub. GitLab
  is reached over ssh with a key that ~/.ssh/config offers for gitlab.com only.
- Arch desktop (2026-10-08): fully updated and on the shared configuration: dotfiles on `main` (the old edits are kept in branch `arch-local`), stowed with `--no-folding`: xmonad polybar bin alacritty tmux rofi dunst gtk qt x11 xdg micro picom starship. Not stowed there: nvim, bash (its own .bashrc), claude, webapps, copyq. Picom is off there (`~/.config/picom/disabled`). alacritty, micro, scrcpy, android-tools and Papirus installed; SDDM with the Catppuccin pink theme on one monitor (`system/arch/sddm-setup.sh`); hot-plug of monitors works through the shared xmonad.hs. Still to do there: its own STATE file (step 5) and trying everything on screen.
- Arch desktop: ssh access from this desktop is set up and claude-tools and webapps are
  installed there; Super+a there is one line in its own xmonad.hs, compiled, and needs
  `xmonad --restart` on that machine to load. Details in home-infra.
- ssh: hardening is planned for this desktop and the Arch desktop (keys first, then passwords
  off and a firewall rule for the LAN only). The current state and the plan are in home-infra.
