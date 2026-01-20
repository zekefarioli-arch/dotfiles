#!/usr/bin/env python3
import os
import re
import subprocess
import sys


def qtile_eval(expr: str) -> str:
    runtime = os.environ.get("XDG_RUNTIME_DIR", "")
    sockets = []
    if runtime:
        sockets = sorted(
            [
                os.path.join(runtime, f)
                for f in os.listdir(runtime)
                if f.startswith("qtile-ipc.")
            ],
            key=lambda p: os.path.getmtime(p),
            reverse=True,
        )

    base = ["qtile"]
    if sockets:
        base += ["-s", sockets[0]]

    p = subprocess.run(
        base + ["cmd-obj", "-o", "cmd", "-f", "eval", "-a", expr],
        capture_output=True,
        text=True,
    )
    if p.returncode != 0:
        return ""
    return p.stdout.strip()


def parse_eval_output(s: str):
    # outputs like: 'True, "...."' or 'False, "traceback..."'
    m = re.match(r"^(True|False),\s*(.*)$", s, re.S)
    if not m:
        return False, s
    ok = m.group(1) == "True"
    payload = m.group(2).strip()
    # strip quotes if present
    if payload.startswith(("'", '"')) and payload.endswith(("'", '"')):
        payload = payload[1:-1]
    return ok, payload


def rofi(lines, prompt="Qtile Help"):
    p = subprocess.run(
        ["rofi", "-dmenu", "-i", "-p", prompt],
        input="\n".join(lines),
        text=True,
        capture_output=True,
    )
    return p.stdout.strip()


def copy_to_clipboard(text):
    # wl-copy if on wayland; xclip/xsel on X11
    for cmd in (
        ["wl-copy"],
        ["xclip", "-selection", "clipboard"],
        ["xsel", "--clipboard", "--input"],
    ):
        try:
            subprocess.run(cmd, input=text, text=True, check=True)
            return True
        except Exception:
            pass
    return False


def main():
    # Get keybindings from the running qtile config.
    expr = r"""
from libqtile.config import Key
def fmt(key):
    mods = "+".join([m.replace("mod4","Super").replace("control","Ctrl").replace("shift","Shift") for m in key.modifiers]) if key.modifiers else ""
    k = key.key
    desc = getattr(key, "desc", "") or ""
    combo = (mods + "+" if mods else "") + k
    return combo + " :: " + desc

items = []
for k in keys:
    if isinstance(k, Key):
        items.append(fmt(k))
print("\n".join(items))
"""
    ok, payload = parse_eval_output(qtile_eval(expr))
    if not ok or not payload:
        print("Could not read keys from qtile. Are you running qtile?")
        sys.exit(1)

    rows = [r for r in payload.split("\n") if "::" in r and r.split("::", 1)[1].strip()]

    # Categorize based on desc keywords (simple + effective)
    def cat(desc: str) -> str:
        d = desc.lower()
        if "focus" in d:
            return "01 Focus"
        if "move window" in d or "shuffle" in d:
            return "02 Move windows"
        if "grow" in d or "resize" in d or "normalize" in d:
            return "03 Resize"
        if "layout" in d:
            return "04 Layouts"
        if "rofi" in d or "launcher" in d:
            return "05 Launcher"
        if "terminal" in d:
            return "06 Apps"
        if "switch to group" in d or "group" in d or "workspace" in d:
            return "07 Groups"
        if "kill" in d or "fullscreen" in d or "floating" in d:
            return "08 Window actions"
        if "reload" in d or "logout" in d or "qtile" in d or "shutdown" in d:
            return "09 Qtile"
        if "screenshot" in d or "flameshot" in d:
            return "10 Screenshot"
        return "99 Other"

    cats = {}
    for r in rows:
        combo, desc = [x.strip() for x in r.split("::", 1)]
        c = cat(desc)
        cats.setdefault(c, []).append((combo, desc))

    # First menu: categories
    cat_list = [c for c in sorted(cats.keys())]
    chosen_cat = rofi(cat_list, prompt="Qtile categories")
    if not chosen_cat:
        return

    # Second menu: entries
    entries = [f"{combo:<18}  {desc}" for combo, desc in cats[chosen_cat]]
    chosen = rofi(entries, prompt=chosen_cat)
    if not chosen:
        return

    # Copy just the combo
    combo = chosen.split()[0]
    copy_to_clipboard(combo)


if __name__ == "__main__":
    main()
