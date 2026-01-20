# ~/.config/qtile/config.py
# Basic Qtile config with Catppuccin Mocha bar + your keybindings.
# All comments in English.

from __future__ import annotations

import os
import subprocess
from typing import List

from libqtile import bar, layout, hook, qtile
from libqtile.config import Key, Group, Screen, Match
from libqtile.lazy import lazy
from libqtile.widget import (
    GroupBox,
    WindowName,
    Spacer,
    Clock,
    Systray,
    Volume,
    Memory,
    CPU,
    Net,
)

# ------------------------------------------------------------
# User preferences
# ------------------------------------------------------------

mod = "mod4"  # Super/Win key
terminal = "kitty"

# Rofi command:
# -drun: app launcher, -show-icons: nicer, -matching fuzzy: "search more"
rofi_cmd = "rofi -show drun -show-icons -matching fuzzy"

# Lock screen command
lock_cmd = "i3lock-fancy"

# Autostart script (optional). Create ~/.config/qtile/autostart.sh and make it executable.
autostart_script = os.path.expanduser("~/.config/qtile/autostart.sh")


# ------------------------------------------------------------
# Catppuccin Mocha palette
# ------------------------------------------------------------

MOCHA = {
    "base": "#1e1e2e",
    "mantle": "#181825",
    "crust": "#11111b",
    "surface0": "#313244",
    "surface1": "#45475a",
    "overlay0": "#6c7086",
    "text": "#cdd6f4",
    "subtext0": "#a6adc8",
    "blue": "#89b4fa",
    "teal": "#94e2d5",
    "pink": "#f5c2e7",
    "red": "#f38ba8",
    "yellow": "#f9e2af",
    "green": "#a6e3a1",
}


# ------------------------------------------------------------
# Keybindings
# ------------------------------------------------------------

keys: List[Key] = [
    # Launchers / essentials
    Key([mod], "Return", lazy.spawn(terminal), desc="Open terminal (kitty)"),
    Key([mod], "r", lazy.spawn(rofi_cmd), desc="Open rofi (fuzzy search)"),
    Key([mod], "l", lazy.spawn(lock_cmd), desc="Lock screen (i3lock-fancy)"),
    # Window management
    Key([mod], "w", lazy.window.kill(), desc="Close focused window"),
    Key([mod], "Tab", lazy.next_layout(), desc="Next layout"),
    # Qtile controls
    Key([mod], "q", lazy.restart(), desc="Restart Qtile (refresh config)"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Logout / shutdown Qtile"),
]

# Optional: basic focus movement (kept minimal)
keys += [
    Key([mod], "h", lazy.layout.left(), desc="Focus left"),
    Key([mod], "l", lazy.layout.right(), desc="Focus right"),
    Key([mod], "j", lazy.layout.down(), desc="Focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Focus up"),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    Key([mod], "space", lazy.layout.toggle_split(), desc="Toggle split (MonadTall)"),
]

# ------------------------------------------------------------
# Groups (workspaces)
# ------------------------------------------------------------

groups = [Group(str(i)) for i in range(1, 9)]

for g in groups:
    keys.append(
        Key(
            [mod],
            g.name,
            lazy.group[g.name].toscreen(),
            desc=f"Switch to group {g.name}",
        )
    )
    keys.append(
        Key(
            [mod, "shift"],
            g.name,
            lazy.window.togroup(g.name),
            desc=f"Move window to group {g.name}",
        )
    )


# ------------------------------------------------------------
# Layouts
# ------------------------------------------------------------

layout_theme = {
    "border_width": 2,
    "margin": 8,
    "border_focus": MOCHA["blue"],
    "border_normal": MOCHA["surface0"],
}

layouts = [
    layout.MonadTall(**layout_theme),
    layout.Max(),
    layout.Columns(**layout_theme),
]

floating_layout = layout.Floating(
    border_focus=MOCHA["pink"],
    border_normal=MOCHA["surface0"],
    border_width=2,
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),
        Match(wm_class="makebranch"),
        Match(wm_class="maketag"),
        Match(wm_class="ssh-askpass"),
        Match(title="branchdialog"),
        Match(title="pinentry"),
    ],
)

# ------------------------------------------------------------
# Widgets (Catppuccin Mocha bar)
# ------------------------------------------------------------


def pill_defaults():
    """Shared styling for a clean Mocha bar."""
    return dict(
        font="JetBrains Mono",
        fontsize=12,
        padding=6,
        background=MOCHA["base"],
        foreground=MOCHA["text"],
    )


widget_defaults = pill_defaults()
extension_defaults = widget_defaults.copy()


def make_widgets(primary_screen: bool = True):
    widgets = [
        GroupBox(
            font="JetBrains Mono",
            fontsize=12,
            padding=6,
            margin_y=4,
            margin_x=6,
            borderwidth=2,
            active=MOCHA["text"],
            inactive=MOCHA["overlay0"],
            highlight_method="block",
            this_current_screen_border=MOCHA["blue"],
            this_screen_border=MOCHA["teal"],
            other_current_screen_border=MOCHA["surface1"],
            other_screen_border=MOCHA["surface0"],
            urgent_border=MOCHA["red"],
            rounded=True,
            disable_drag=True,
            background=MOCHA["base"],
        ),
        Spacer(length=8),
        WindowName(
            max_chars=60,
            foreground=MOCHA["subtext0"],
        ),
        Spacer(),
        Net(
            interface="auto",
            format="{interface} {down}↓ {up}↑",
            foreground=MOCHA["subtext0"],
        ),
        Spacer(length=10),
        Volume(
            fmt="VOL {}",
            foreground=MOCHA["blue"],
        ),
        Spacer(length=10),
        Memory(
            format="RAM {MemPercent: .0f}%",
            foreground=MOCHA["teal"],
        ),
        Spacer(length=10),
        CPU(
            format="CPU {load_percent: .0f}%",
            foreground=MOCHA["pink"],
        ),
        Spacer(length=10),
        Clock(
            format="%H:%M",
            foreground=MOCHA["yellow"],
        ),
    ]

    # Systray should only be on one screen
    if primary_screen:
        widgets.insert(-1, Spacer(length=10))
        widgets.insert(-1, Systray(icon_size=18, padding=6))

    return widgets


screens = [
    Screen(
        top=bar.Bar(
            make_widgets(primary_screen=True),
            size=28,
            background=MOCHA["base"],
            opacity=1.0,
            margin=[0, 0, 0, 0],
        )
    ),
    Screen(
        top=bar.Bar(
            make_widgets(primary_screen=False),
            size=28,
            background=MOCHA["base"],
            opacity=1.0,
            margin=[0, 0, 0, 0],
        )
    ),
]


# ------------------------------------------------------------
# Autostart
# ------------------------------------------------------------


@hook.subscribe.startup_once
def _autostart():
    """Run the user's autostart script once per login."""
    if os.path.exists(autostart_script):
        subprocess.Popen(["bash", autostart_script])


# ------------------------------------------------------------
# Required Qtile settings
# ------------------------------------------------------------

dgroups_key_binder = None
dgroups_app_rules = []

follow_mouse_focus = True
bring_front_click = False
cursor_warp = False

auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

wmname = "LG3D"  # Helps some Java apps

# Mouse support (basic)
mouse = []

# If you want to reduce CPU usage further, keep these minimal:
auto_minimize = True
