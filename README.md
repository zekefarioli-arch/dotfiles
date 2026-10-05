# dotfiles

These are the dotfiles of my ThinkPad X13 Gen 1 (AMD) running a minimal Fedora 44 with xmonad, Polybar and the Catppuccin Mocha theme with a pink accent. I keep the system as light as possible: every tool is small and single-purpose, and nothing from a full desktop environment is installed unless I need it.

## Layout and installation

Each top-level folder is a [GNU Stow](https://www.gnu.org/software/stow/) package that mirrors the home directory, so `xmonad/.xmonad/xmonad.hs` ends up as `~/.xmonad/xmonad.hs`. I install them with `--no-folding`, which creates real directories and links only the files; thus, programs can still write their own files next to mine.

```sh
git clone https://github.com/zekefarioli-arch/dotfiles.git ~/dotfiles
cd ~/dotfiles
stow --no-folding -t ~ xmonad polybar bin alacritty tmux nvim rofi dunst gtk qt x11 xdg
```

| Package | What it configures |
|---|---|
| `xmonad` | window manager, action catalog, `keys.conf`, autostart |
| `polybar` | one bar per monitor, fed by xmonad |
| `bin` | scripts in `~/.local/bin` (shortcut tools, lock screen, OSD, phone) |
| `alacritty`, `tmux`, `starship`, `bash` | terminal, dropdown session and prompt |
| `nvim` | LazyVim for Java, Erlang, Elixir, TypeScript and front end |
| `rofi`, `dunst`, `gtk`, `qt`, `copyq` | launcher, notifications and Catppuccin theming |
| `x11`, `xdg` | X session environment, default apps, user folders |
| `system` | files outside `$HOME`; each one explains in its header how to install it |

Two scripts set up the rest of a fresh Fedora install: `scripts/fedora-post-install.sh` (codecs, VA-API, snapshots, power profiles) and `scripts/install-themes.sh` (GTK, Qt, icons and cursor). The `qtile` package is my old setup; I keep it for reference, but it is not maintained.

## Shortcuts

I wanted to see every shortcut in one place, run them from that list and change them without editing Haskell. For this reason, xmonad's bindings are split into three parts.

First, `xmonad.hs` defines a catalog of **actions**. Each action has a category, a stable name such as `close-window`, a description, its default shortcuts and the code it runs. The catalog also includes actions with no shortcut, such as "move the window to workspace 3 and follow it". At startup, xmonad exports the catalog to `~/.cache/xmonad/actions.tsv`.

Second, `~/.xmonad/keys.conf` says which shortcuts each action has, one line per action:

```
close-window              M-w  M-S-c
swap-master
```

An action missing from the file keeps its defaults, and an action listed alone has none. xmonad reads the file at startup, so a change only needs `xmonad --restart`, which takes about a second and keeps the windows open. If the file contains an unknown action, an invalid shortcut or a shortcut used twice, xmonad still starts and reports the problem in a notification.

Third, two rofi tools work on top of these files:

| Keys | Tool | What it does |
|---|---|---|
| `Super + F1` or `Super + /` | `keybinds` | Lists every shortcut by category, including tmux, Neovim and other apps (`app-keys.tsv`). Enter runs the selected shortcut on the window I was using. |
| `Super + Shift + F1` or `Super + Shift + /` | `keys-editor` | Changes, adds (up to two per action), removes or resets shortcuts. I press the new combination instead of typing it. |

The editor captures the combination by grabbing the keyboard through libX11, so it works even for combinations that xmonad already uses. If the new shortcut belongs to another action, it asks before moving it; if it has no modifier, it warns me, because typing that key anywhere would run the action. The editor also has a small command line (`keys-editor list`, `set`, `remove`, `reset`, `free`).

When the cheat sheet runs an xmonad shortcut, it replays it by keycode. I learnt this the hard way: simulating `Ctrl + F5` with `xdotool key` added Alt, sent `Ctrl + Alt + F5` and switched me to a text console.

Besides the window manager actions, the catalog has screenshots of an area, the active window, the monitor under the mouse or every monitor. Each screenshot is copied to the clipboard and saved in `~/Pictures/Screenshots`, and there are a few utilities as well, such as suspend, the browser, the phone screen and the microphone. For anything else, the editor's **+ New action** asks for a description and a shell command and saves it in `~/.xmonad/actions.conf`. For instance, an action that runs `flatpak run com.spotify.Client` then gets a shortcut like any built-in one, and it can be renamed, changed or deleted from the editor.

To add a built-in action, I add an `Action` line to the catalog in `xmonad.hs` and recompile with `Super + Q`; after that, the editor and the cheat sheet show it automatically.

The shared Python code lives in `bin/.local/lib/xmonad-keys/xkeys.py`, and the tests cover the conversions, the editor's changes to `keys.conf` and the cheat sheet rows. They only use the standard library and temporary files:

```sh
python3 -m unittest discover -s tests -v
```

The main limitation is that a shortcut for another app, such as tmux or Neovim, only works from the cheat sheet if that app was the active window when I opened it.

## Polybar

xmonad starts the bars itself. At startup and whenever a monitor is connected or disconnected, it runs `launch-bars.sh`, which kills every bar and starts one per monitor. Starting from scratch each time means that no bar is left behind for a monitor that is gone. The bar on the first monitor carries the system tray.

Each bar reads its own screen's text, which xmonad publishes in the `_XMONAD_LOG_N` root property. It shows the focused-monitor block (hidden with a single monitor), the workspaces, the layout, the focused window and an orange badge while the dropdown terminal covers the screen. `Super + B`, `Super + Ctrl + B` and `Super + Shift + B` hide the main bar, the secondary bar and all bars.

I have only tested the hot-plugging with simulated monitors so far, so my next step is to confirm it with a real external monitor.
