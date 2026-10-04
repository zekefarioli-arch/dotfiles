"""Tests for the shortcut tools (xkeys, keys-editor, keybinds).

Run from the repo root:  python3 -m unittest discover -s tests -v
They use temporary files only: the real keys.conf is never touched and
xmonad is never restarted. Tests that need the X display are skipped
without one.
"""
import importlib.machinery
import importlib.util
import os
import sys
import tempfile
import types
import unittest
from unittest import mock

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BIN = os.path.join(REPO, "bin", ".local", "bin")
sys.path.insert(0, os.path.join(REPO, "bin", ".local", "lib", "xmonad-keys"))
import xkeys  # noqa: E402


def load_script(name):
    """Import a script without a .py extension as a module."""
    path = os.path.join(BIN, name)
    loader = importlib.machinery.SourceFileLoader(name.replace("-", "_"), path)
    module = types.ModuleType(loader.name)
    module.__file__ = path
    loader.exec_module(module)
    return module


editor = load_script("keys-editor")
keybinds = load_script("keybinds")

ACTIONS_TSV = "\n".join([
    "Help\tshow-shortcuts\tShow the shortcut list\tM-<F1> M-/\tM-<F1> M-/",
    "Apps\tterminal\tTerminal\tM-<Return>\tM-<Return>",
    "Apps\tlauncher\tLauncher\tM-r\tM-r",
    "Apps\tclipboard\tClipboard\tM-v\tM-v",
    "Windows\tclose-window\tClose the focused window\tM-w M-S-c\tM-w M-S-c",
    "Windows\tswap-master\tSwap with the master window\t\t",
    "Windows\tsink-window\tSink\tM-S-t\tM-S-t",
]) + "\n"

KEYS_CONF = """# comment line
#
# Help
show-shortcuts            M-<F1>  M-/

# Apps
terminal                  M-<Return>
launcher                  M-r
clipboard                 M-v

# Windows
close-window              M-w  M-S-c
swap-master
"""


class ConversionTests(unittest.TestCase):
    def test_split(self):
        self.assertEqual(xkeys.split("M-S-x"), (["M", "S"], "x"))
        self.assertEqual(xkeys.split("M-<Return>"), (["M"], "<Return>"))
        self.assertEqual(xkeys.split("<Print>"), ([], "<Print>"))
        self.assertEqual(xkeys.split("M--"), (["M"], "-"))

    def test_canonical_orders_and_dedupes_modifiers(self):
        self.assertEqual(xkeys.canonical("S-M-x"), "M-S-x")
        self.assertEqual(xkeys.canonical("M1-C-M-x"), "M-C-M1-x")
        self.assertEqual(xkeys.canonical("M-M-x"), "M-x")
        self.assertEqual(xkeys.canonical("x"), "x")

    def test_to_pretty(self):
        self.assertEqual(xkeys.to_pretty("M-S-x"), "Super + Shift + X")
        self.assertEqual(xkeys.to_pretty("M-<Return>"), "Super + Enter")
        self.assertEqual(xkeys.to_pretty("M-,"), "Super + ,")
        self.assertEqual(xkeys.to_pretty("<XF86AudioMute>"), "Mute")
        self.assertEqual(xkeys.to_pretty("C-<F5>"), "Ctrl + F5")
        self.assertEqual(xkeys.to_pretty("S-M-x"), "Super + Shift + X")
        self.assertEqual(xkeys.to_pretty("M-M-x"), "Super + X")

    def test_to_xdotool(self):
        self.assertEqual(xkeys.to_xdotool("M-S-x"), "super+shift+x")
        self.assertEqual(xkeys.to_xdotool("M-,"), "super+comma")
        self.assertEqual(xkeys.to_xdotool("M-/"), "super+slash")
        self.assertEqual(xkeys.to_xdotool("M-<Space>"), "super+space")
        self.assertEqual(xkeys.to_xdotool("C-<Page_Up>"), "ctrl+Prior")
        self.assertEqual(xkeys.to_xdotool("M-1"), "super+1")

    @unittest.skipUnless(os.environ.get("DISPLAY"), "needs the X display")
    def test_keycodes_use_real_f_keys_without_alt(self):
        codes = xkeys.to_keycodes("C-<F5>")
        self.assertEqual(len(codes), 2)          # Ctrl and F5 only, no Alt added
        self.assertNotIn(64, codes)              # Alt_L


class EditorTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        d = self.tmp.name
        self.actions_file = os.path.join(d, "actions.tsv")
        self.real_keys = os.path.join(d, "repo-keys.conf")
        self.keys_link = os.path.join(d, "keys.conf")   # stow-style symlink
        with open(self.actions_file, "w") as f:
            f.write(ACTIONS_TSV)
        with open(self.real_keys, "w") as f:
            f.write(KEYS_CONF)
        os.symlink(self.real_keys, self.keys_link)
        self.patches = [
            mock.patch.object(editor, "ACTIONS_FILE", self.actions_file),
            mock.patch.object(editor, "KEYS_FILE", self.keys_link),
            mock.patch.object(editor.subprocess, "run"),   # no xmonad --restart
        ]
        for p in self.patches:
            p.start()
        self.actions = editor.load_actions()

    def tearDown(self):
        for p in self.patches:
            p.stop()
        self.tmp.cleanup()

    def conf(self):
        with open(self.keys_link) as f:
            return f.read()

    def line(self, name):
        return next(l for l in self.conf().splitlines() if l.split() and l.split()[0] == name)

    def test_current_keys_come_from_keys_conf_not_the_catalog(self):
        with open(self.real_keys, "a") as f:
            f.write("sink-window\n")                  # listed alone: no shortcut
        a = editor.find(editor.load_actions(), "sink-window")
        self.assertEqual(a.keys, [])
        self.assertEqual(a.default, ["M-S-t"])

    def test_missing_action_keeps_defaults(self):
        self.assertEqual(editor.find(self.actions, "sink-window").keys, ["M-S-t"])

    def test_set_adds_a_shortcut_and_restarts_xmonad(self):
        a = editor.find(self.actions, "swap-master")
        self.assertIsNone(editor.set_key(self.actions, a, 1, "S-M-m"))
        self.assertEqual(self.line("swap-master").split(), ["swap-master", "M-S-m"])
        editor.subprocess.run.assert_called_with(["xmonad", "--restart"], check=False)

    def test_set_replaces_a_slot(self):
        a = editor.find(self.actions, "close-window")
        editor.set_key(self.actions, a, 2, "M-q")
        self.assertEqual(self.line("close-window").split()[1:], ["M-w", "M-q"])

    def test_conflict_is_reported_without_changes(self):
        before = self.conf()
        a = editor.find(self.actions, "clipboard")
        other = editor.set_key(self.actions, a, 2, "M-r")
        self.assertEqual(other.name, "launcher")
        self.assertEqual(self.conf(), before)

    def test_steal_moves_the_shortcut(self):
        a = editor.find(self.actions, "clipboard")
        editor.set_key(self.actions, a, 2, "M-r", steal=True)
        self.assertEqual(self.line("clipboard").split()[1:], ["M-v", "M-r"])
        self.assertEqual(self.line("launcher").split(), ["launcher"])

    def test_reset_takes_defaults_back_from_other_actions(self):
        a = editor.find(self.actions, "clipboard")
        editor.set_key(self.actions, a, 2, "M-r", steal=True)
        editor.reset_keys(self.actions, editor.find(self.actions, "launcher"))
        self.assertEqual(self.line("launcher").split()[1:], ["M-r"])
        self.assertEqual(self.line("clipboard").split()[1:], ["M-v"])

    def test_remove(self):
        a = editor.find(self.actions, "close-window")
        editor.remove_key(self.actions, a, 1)
        self.assertEqual(self.line("close-window").split()[1:], ["M-S-c"])

    def test_round_trip_keeps_the_file_identical(self):
        before = self.conf()
        a = editor.find(self.actions, "clipboard")
        editor.set_key(self.actions, a, 2, "M-r", steal=True)
        editor.reset_keys(self.actions, editor.find(self.actions, "launcher"))
        s = editor.find(self.actions, "swap-master")
        editor.set_key(self.actions, s, 1, "M-S-m")
        editor.remove_key(self.actions, s, 1)
        self.assertEqual(self.conf(), before)

    def test_new_action_goes_under_its_category(self):
        a = editor.find(self.actions, "sink-window")      # not in keys.conf yet
        editor.set_key(self.actions, a, 1, "M-S-y")
        lines = self.conf().splitlines()
        i = lines.index("# Windows")
        self.assertIn(f"{'sink-window':<{editor.NAME_WIDTH}}  M-S-y", lines[i:])

    def test_save_writes_through_the_symlink(self):
        editor.set_key(self.actions, editor.find(self.actions, "swap-master"), 1, "M-S-m")
        self.assertTrue(os.path.islink(self.keys_link))
        self.assertFalse(os.path.exists(self.real_keys + ".tmp"))

    def test_free_shortcuts_are_unused(self):
        used = {xkeys.canonical(k) for a in self.actions for k in a.keys}
        free = editor.free_shortcuts(self.actions, 20)
        self.assertEqual(len(free), 20)
        self.assertFalse(used & set(free))


class CheatSheetTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        d = self.tmp.name
        paths = {"ACTIONS_FILE": "actions.tsv", "KEYS_FILE": "keys.conf", "APP_FILE": "app-keys.tsv"}
        contents = {"actions.tsv": ACTIONS_TSV, "keys.conf": KEYS_CONF + "launcher  M-S-r\n",
                    "app-keys.tsv": "# comment\ntmux\tCtrl+B  then  C\tNew tmux window\tkey:ctrl+b c\n"
                                    "Mouse\tSuper + left drag\tMove a window\t\n"}
        self.patches = []
        for attr, fname in paths.items():
            with open(os.path.join(d, fname), "w") as f:
                f.write(contents[fname])
            self.patches.append(mock.patch.object(keybinds, attr, os.path.join(d, fname)))
        for p in self.patches:
            p.start()

    def tearDown(self):
        for p in self.patches:
            p.stop()
        self.tmp.cleanup()

    def test_rows_follow_keys_conf(self):
        rows = {r["desc"]: r for r in keybinds.load_rows()}
        self.assertEqual(rows["Launcher"]["keys"], "Super + Shift + R")   # last line wins
        self.assertEqual(rows["Swap with the master window"]["keys"], "")
        self.assertEqual(rows["Sink"]["keys"], "Super + Shift + T")       # default
        self.assertIn("New tmux window", rows)

    def test_runnable_marks(self):
        rows = {r["desc"]: r for r in keybinds.load_rows()}
        self.assertFalse(keybinds.runnable(rows["Show the shortcut list"]))
        self.assertTrue(keybinds.runnable(rows["Swap with the master window"]))  # opens the editor
        self.assertTrue(keybinds.runnable(rows["New tmux window"]))
        self.assertFalse(keybinds.runnable(rows["Move a window"]))

    def test_render_escapes_markup(self):
        out = keybinds.render([{"cat": "X", "desc": "a <b> & c", "keys": "Ctrl+<", "action": "cmd:true"}])
        self.assertIn("a &lt;b&gt; &amp; c", out[0])
        self.assertIn("Ctrl+&lt;", out[0])


if __name__ == "__main__":
    unittest.main()
