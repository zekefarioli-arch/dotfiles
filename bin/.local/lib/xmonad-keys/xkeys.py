"""Shortcut helpers shared by key-capture, keys-editor and keybinds.

Shortcuts are stored in xmonad's EZConfig syntax ("M-S-x", "M-<Return>").
This module converts them to a readable form ("Super + Shift + X") and to
xdotool syntax ("super+shift+x"), and captures a real key combination by
grabbing the keyboard through libX11 (ctypes, no extra packages).
"""
import ctypes
import re
import ctypes.util
import select
import subprocess
import time

# EZConfig modifier prefixes, in the canonical order used when writing
MODIFIERS = [  # (EZConfig, X mask, readable, xdotool)
    ("M", 1 << 6, "Super", "super"),
    ("C", 1 << 2, "Ctrl", "ctrl"),
    ("S", 1 << 0, "Shift", "shift"),
    ("M1", 1 << 3, "Alt", "alt"),
    ("M5", 1 << 7, "AltGr", "ISO_Level3_Shift"),
]

# X keysym names whose EZConfig name differs (EZConfig uses <Name>)
X_TO_EZ = {"BackSpace": "Backspace", "space": "Space", "Prior": "Page_Up",
           "Next": "Page_Down", "Scroll_Lock": "Scroll_lock"}
EZ_TO_X = {v: k for k, v in X_TO_EZ.items()}

# Readable names for special keys
PRETTY = {"Return": "Enter", "Space": "Space", "Backspace": "Backspace",
          "Escape": "Esc", "Page_Up": "Page Up", "Page_Down": "Page Down",
          "XF86AudioMute": "Mute", "XF86AudioRaiseVolume": "Volume Up",
          "XF86AudioLowerVolume": "Volume Down", "XF86MonBrightnessUp": "Brightness Up",
          "XF86MonBrightnessDown": "Brightness Down", "Print": "Print Screen"}

MODIFIER_KEYSYMS = {"Shift_L", "Shift_R", "Control_L", "Control_R", "Super_L", "Super_R",
                    "Alt_L", "Alt_R", "Meta_L", "Meta_R", "Hyper_L", "Hyper_R",
                    "ISO_Level3_Shift", "ISO_Level5_Shift", "Caps_Lock", "Num_Lock", "Mode_switch"}


def split(ez):
    """Split an EZConfig shortcut into (modifier list, key name)."""
    parts = ez.split("-")
    mods = []
    # The key can itself be "-" ("M--"), so stop before the last part
    while len(parts) > 1 and parts[0] in {m[0] for m in MODIFIERS}:
        mods.append(parts.pop(0))
    key = "-".join(parts)
    return mods, key


def key_name(key):
    """Key part of an EZConfig shortcut without the angle brackets."""
    return key[1:-1] if key.startswith("<") and key.endswith(">") and len(key) > 2 else key


def canonical(ez):
    """Normalise modifier order so equal shortcuts compare equal."""
    mods, key = split(ez)
    order = [m[0] for m in MODIFIERS]
    mods = sorted(set(mods), key=order.index)
    return "-".join(mods + [key])


def to_pretty(ez):
    mods, key = split(canonical(ez))
    names = {m[0]: m[2] for m in MODIFIERS}
    k = key_name(key)
    k = PRETTY.get(k, k.upper() if len(k) == 1 else k)
    return " + ".join([names[m] for m in mods] + [k])


def to_xdotool(ez):
    mods, key = split(canonical(ez))
    names = {m[0]: m[3] for m in MODIFIERS}
    k = key_name(key)
    if len(k) == 1:  # a printable character: xdotool wants its keysym name
        k = _x().keysym_name(ord(k)) if k.isascii() and not k.isalnum() else k
    else:
        k = EZ_TO_X.get(k, k)
    return "+".join([names[m] for m in mods] + [k])


# Words accepted when a shortcut is typed ("Super+Shift+X", "ctrl + alt + k")
MOD_WORDS = {"super": "M", "win": "M", "windows": "M", "mod4": "M", "mod": "M",
             "ctrl": "C", "control": "C", "shift": "S", "alt": "M1", "meta": "M1", "altgr": "M5"}
KEY_WORDS = {"enter": "Return", "return": "Return", "space": "Space", "spacebar": "Space",
             "tab": "Tab", "esc": "Escape", "escape": "Escape", "backspace": "Backspace",
             "del": "Delete", "delete": "Delete", "ins": "Insert", "insert": "Insert",
             "home": "Home", "end": "End", "pageup": "Page_Up", "pgup": "Page_Up",
             "pagedown": "Page_Down", "pgdn": "Page_Down", "up": "Up", "down": "Down",
             "left": "Left", "right": "Right", "print": "Print", "printscreen": "Print",
             "prtsc": "Print", "mute": "XF86AudioMute", "volumeup": "XF86AudioRaiseVolume",
             "volumedown": "XF86AudioLowerVolume", "brightnessup": "XF86MonBrightnessUp",
             "brightnessdown": "XF86MonBrightnessDown", "comma": ",", "period": ".",
             "dot": ".", "slash": "/", "minus": "-", "equal": "=", "semicolon": ";"}
EZ_PATTERN = re.compile(r"((M|C|S|M1|M5)-)*(<[A-Za-z0-9_]+>|[!-~])")


def _valid_named(name):
    """True if name is a key xmonad can bind (an X keysym name)."""
    return bool(_x().lib.XStringToKeysym(EZ_TO_X.get(name, name).encode()))


def parse_typed(text):
    """EZConfig shortcut for a typed one ("Super+Shift+X", "ctrl+alt+k",
    "alt+F4", "M-S-x"), or None if it is not a valid shortcut."""
    t = text.strip()
    if not t:
        return None
    if EZ_PATTERN.fullmatch(t):  # already EZConfig syntax
        mods, key = split(t)
        if key.startswith("<"):
            if not _valid_named(key[1:-1]):
                return None
        elif key.isalpha():
            key = key.lower()  # xmonad matches the unshifted keysym
        return canonical("-".join(mods + [key]))
    *mods, key = [p.strip() for p in t.split("+")]
    if not key or any(not m for m in mods):
        return None
    try:
        ez = [MOD_WORDS[m.lower()] for m in mods]
    except KeyError:
        return None
    word = key.lower().replace(" ", "")
    if len(key) == 1 and key.isprintable() and key != " ":
        k = key.lower()
    elif word in KEY_WORDS:
        v = KEY_WORDS[word]
        k = v if len(v) == 1 else f"<{v}>"
    elif re.fullmatch(r"f([1-9]|1[0-9]|2[0-4])", word):
        k = f"<F{word[1:]}>"
    elif _valid_named(key):
        k = f"<{key}>"
    else:
        return None
    return canonical("-".join(ez + [k]))


# ---------------------------------------------------------------------------
# libX11 access
# ---------------------------------------------------------------------------

class _XKeyEvent(ctypes.Structure):
    _fields_ = [("type", ctypes.c_int), ("serial", ctypes.c_ulong), ("send_event", ctypes.c_int),
                ("display", ctypes.c_void_p), ("window", ctypes.c_ulong), ("root", ctypes.c_ulong),
                ("subwindow", ctypes.c_ulong), ("time", ctypes.c_ulong), ("x", ctypes.c_int),
                ("y", ctypes.c_int), ("x_root", ctypes.c_int), ("y_root", ctypes.c_int),
                ("state", ctypes.c_uint), ("keycode", ctypes.c_uint), ("same_screen", ctypes.c_int)]


class _XEvent(ctypes.Union):
    _fields_ = [("type", ctypes.c_int), ("xkey", _XKeyEvent), ("pad", ctypes.c_long * 24)]


class _X:
    KEY_PRESS, KEY_RELEASE = 2, 3

    def __init__(self):
        lib = ctypes.cdll.LoadLibrary(ctypes.util.find_library("X11"))
        lib.XOpenDisplay.restype = ctypes.c_void_p
        lib.XOpenDisplay.argtypes = [ctypes.c_char_p]
        lib.XDefaultRootWindow.restype = ctypes.c_ulong
        lib.XDefaultRootWindow.argtypes = [ctypes.c_void_p]
        lib.XGrabKeyboard.argtypes = [ctypes.c_void_p, ctypes.c_ulong, ctypes.c_int,
                                      ctypes.c_int, ctypes.c_int, ctypes.c_ulong]
        lib.XUngrabKeyboard.argtypes = [ctypes.c_void_p, ctypes.c_ulong]
        lib.XPending.argtypes = [ctypes.c_void_p]
        lib.XNextEvent.argtypes = [ctypes.c_void_p, ctypes.POINTER(_XEvent)]
        lib.XConnectionNumber.argtypes = [ctypes.c_void_p]
        lib.XkbKeycodeToKeysym.restype = ctypes.c_ulong
        lib.XkbKeycodeToKeysym.argtypes = [ctypes.c_void_p, ctypes.c_ubyte, ctypes.c_int, ctypes.c_int]
        lib.XKeysymToString.restype = ctypes.c_char_p
        lib.XKeysymToString.argtypes = [ctypes.c_ulong]
        lib.XStringToKeysym.restype = ctypes.c_ulong
        lib.XStringToKeysym.argtypes = [ctypes.c_char_p]
        lib.XKeysymToKeycode.restype = ctypes.c_ubyte
        lib.XKeysymToKeycode.argtypes = [ctypes.c_void_p, ctypes.c_ulong]
        lib.XFlush.argtypes = [ctypes.c_void_p]
        lib.XCloseDisplay.argtypes = [ctypes.c_void_p]
        self.lib = lib

    def keysym_name(self, keysym):
        name = self.lib.XKeysymToString(keysym)
        return name.decode() if name else None


_x_instance = None


def _x():
    global _x_instance
    if _x_instance is None:
        _x_instance = _X()
    return _x_instance


# Keycodes of the left modifier keys, pressed when replaying a shortcut
MODIFIER_KEYSYM_NAMES = {"M": "Super_L", "C": "Control_L", "S": "Shift_L",
                         "M1": "Alt_L", "M5": "ISO_Level3_Shift"}


def to_keycodes(ez):
    """Keycodes to press, in order, to replay an EZConfig shortcut. Replaying
    by keycode avoids xdotool's keysym remapping, which adds Alt to F-keys
    (Ctrl+F5 would become Ctrl+Alt+F5 and switch the virtual terminal)."""
    x = _x()
    dpy = x.lib.XOpenDisplay(None)
    if not dpy:
        raise RuntimeError("cannot open the X display")
    try:
        mods, key = split(canonical(ez))
        k = key_name(key)
        names = [MODIFIER_KEYSYM_NAMES[m] for m in mods]
        names.append(x.keysym_name(ord(k)) if len(k) == 1 else EZ_TO_X.get(k, k))
        codes = [x.lib.XKeysymToKeycode(dpy, x.lib.XStringToKeysym(n.encode())) for n in names]
        if 0 in codes:
            raise ValueError(f"no keycode for {ez}")
        return codes
    finally:
        x.lib.XCloseDisplay(dpy)


def replay(ez):
    """Press and release an EZConfig shortcut with xdotool, by keycode."""
    codes = to_keycodes(ez)
    args = ["xdotool"]
    args += [a for c in codes for a in ("keydown", str(c))]
    args += [a for c in reversed(codes) for a in ("keyup", str(c))]
    subprocess.run(args, check=False)


def _ez_from_event(x, dpy, ev):
    """EZConfig shortcut for a KeyPress, or None for a lone modifier key."""
    # Level 0 keysym, as xmonad matches keys (Shift+, is "M-S-," not "M-<")
    keysym = x.lib.XkbKeycodeToKeysym(dpy, ev.keycode, 0, 0)
    name = x.keysym_name(keysym)
    if not name or name in MODIFIER_KEYSYMS:
        return None
    if keysym < 0x7f and chr(keysym).isprintable() and keysym != 0x20:
        key = chr(keysym)
    else:
        key = "<" + X_TO_EZ.get(name, name) + ">"
    mods = [m[0] for m in MODIFIERS if ev.state & m[1]]
    return "-".join(mods + [key])


def _notify(body, timeout_ms):
    subprocess.run(["notify-send", "-a", "osd", "-u", "low", "-t", str(timeout_ms),
                    "-h", "string:x-dunst-stack-tag:key-capture", "-i", "input-keyboard",
                    "Shortcut", body], check=False)


def capture(timeout=10.0, notify=True):
    """Grab the keyboard and return the next key combination in EZConfig
    syntax, or None if Esc is pressed, the time runs out or the grab fails."""
    x = _x()
    dpy = x.lib.XOpenDisplay(None)
    if not dpy:
        raise RuntimeError("cannot open the X display")
    root = x.lib.XDefaultRootWindow(dpy)
    try:
        # Another client (e.g. rofi closing) may still hold the keyboard
        for _ in range(40):
            if x.lib.XGrabKeyboard(dpy, root, 0, 1, 1, 0) == 0:  # GrabModeAsync, CurrentTime
                break
            time.sleep(0.05)
        else:
            return None
        x.lib.XFlush(dpy)
        if notify:
            _notify("Press the new shortcut  (Esc to cancel)", int(timeout * 1000))
        fd = x.lib.XConnectionNumber(dpy)
        deadline = time.monotonic() + timeout
        ev = _XEvent()
        result = None
        while time.monotonic() < deadline:
            if not x.lib.XPending(dpy):
                select.select([fd], [], [], max(0.0, deadline - time.monotonic()))
                continue
            x.lib.XNextEvent(dpy, ctypes.byref(ev))
            if ev.type != x.KEY_PRESS:
                continue
            ez = _ez_from_event(x, dpy, ev.xkey)
            if ez is None:
                continue
            result = None if ez == "<Escape>" else ez
            break
        if notify:
            _notify(f"Captured: {to_pretty(result)}" if result else "Cancelled", 1500)
        return result
    finally:
        x.lib.XUngrabKeyboard(dpy, 0)
        x.lib.XFlush(dpy)
        x.lib.XCloseDisplay(dpy)
