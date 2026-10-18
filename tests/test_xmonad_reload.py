"""Tests for xmonad-reload: reading GHC's errors, the message, and the real recompile.

Run from the repo root:  python3 -m unittest discover -s tests -v
The samples of GHC output are real (captured from xmonad 0.18 / ghc). The last class compiles tiny
xmonad configurations in a temporary folder with the real xmonad and ghc (skipped if missing); it
never restarts anything and never touches ~/.xmonad.
"""
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

sys.dont_write_bytecode = True
REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(REPO, "bin", ".local", "lib", "xmonad-reload"))
import xmreload  # noqa: E402

NOISE = """XMonad will use ghc to recompile, because none of "/CFG/build", "/CFG/stack.yaml" exist.
XMonad recompiling (forced).
Errors detected while compiling xmonad config: /CFG/xmonad.hs
$ ghc --make xmonad.hs -i -ilib -fforce-recomp -main-is main -v0 -outputdir /CFG/cache -o /CFG/cache/xmonad
"""
PARSE = NOISE + """xmonad.hs:399:5: error: [GHC-58481] parse error on input ‘]’
    |
399 |     ]
    |     ^


Please check the file for errors.
"""
SCOPE = NOISE + """xmonad.hs:5:26: error: [GHC-88464]
    Variable not in scope: termnal :: String
    Suggested fix:
      Perhaps use record field of XConfig ‘terminal’ (imported from XMonad)
  |
5 |          , ("M-t", spawn termnal)
  |                          ^^^^^^^


Please check the file for errors.
"""
MANY = NOISE + """xmonad.hs:3:1: warning: [GHC-38417] [-Wmissing-signatures]
    Top-level binding with no type signature: x :: Int
xmonad.hs:5:26: error: [GHC-88464]
    Variable not in scope: termnal :: String
  |
5 |          , ("M-t", spawn termnal)
  |
xmonad.hs:(8,3)-(9,20): error: [GHC-83865]
    • Couldn't match type ‘Integer’ with ‘[Char]’
      Expected: String
        Actual: Integer
    • In the second argument of ‘f’
  |
8 |   , terminal = 42
  |

Please check the file for errors.
"""


class Parsing(unittest.TestCase):
    def test_a_parse_error(self):
        (e,) = xmreload.parse_errors(PARSE)
        self.assertEqual((e["file"], e["line"], e["col"], e["code"]), ("xmonad.hs", 399, 5, "GHC-58481"))
        self.assertEqual(e["message"], "parse error on input ‘]’")
        self.assertEqual(e["detail"], [])  # GHC's own excerpt is dropped: we print our own

    def test_a_scope_error_keeps_its_explanation(self):
        (e,) = xmreload.parse_errors(SCOPE)
        self.assertEqual(e["message"], "Variable not in scope: termnal :: String")
        self.assertEqual([d.strip() for d in e["detail"]],
                         ["Suggested fix:", "Perhaps use record field of XConfig ‘terminal’ (imported from XMonad)"])

    def test_many_errors_in_order_warnings_ignored_spans_read(self):
        errors = xmreload.parse_errors(MANY)
        self.assertEqual([(e["line"], e["col"]) for e in errors], [(5, 26), (8, 3)])
        self.assertEqual(errors[1]["code"], "GHC-83865")
        self.assertIn("Couldn't match type", errors[1]["message"] + " ".join(errors[1]["detail"]))
        self.assertTrue(any("Expected: String" in d for d in errors[1]["detail"]))

    def test_nothing_recognisable(self):
        self.assertEqual(xmreload.parse_errors("ghc: panic! the impossible happened"), [])
        self.assertEqual(xmreload.raw_failure(NOISE + "ghc: panic!\n"), "ghc: panic!")
        self.assertIn("without a message", xmreload.raw_failure(NOISE))

    def test_the_message_may_start_on_the_next_line(self):
        (e,) = xmreload.parse_errors("xmonad.hs:2:1: error:\n    Parse error: module header\n")
        self.assertEqual(e["message"], "Parse error: module header")


class Message(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.dir)
        with open(os.path.join(self.dir, "xmonad.hs"), "w") as f:
            f.write("".join(f"line {n}\n" for n in range(1, 11)))

    def test_context_lines(self):
        path = os.path.join(self.dir, "xmonad.hs")
        self.assertEqual([n for n, _ in xmreload.source_context(path, 5)], [1, 2, 3, 4, 5, 6])
        self.assertEqual([n for n, _ in xmreload.source_context(path, 2)], [1, 2, 3])
        self.assertEqual([n for n, _ in xmreload.source_context(path, 10)], [6, 7, 8, 9, 10])
        # An error "at the end of the file" points one line past the last: show where that is.
        past = xmreload.source_context(path, 11)
        self.assertEqual(past[-1], (11, "⟨end of the file⟩"))
        self.assertEqual(past[0][0], 7)
        self.assertEqual(xmreload.source_context(os.path.join(self.dir, "none.hs"), 3), [])

    def test_plain_message(self):
        errors = xmreload.parse_errors(PARSE.replace("399:5", "7:5"))
        text = xmreload.render(errors, self.dir, "plain")
        self.assertIn("✗ xmonad configuration has 1 error\n", text)
        self.assertIn("was NOT restarted", text)
        self.assertIn("1. xmonad.hs:7:5  [GHC-58481]", text)
        self.assertIn("parse error on input ‘]’", text)
        self.assertIn("▶    7 │ line 7", text)  # the line of the error is marked
        self.assertIn("      6 │ line 6", text)
        self.assertIn("hint: A parse error is often reported lines AFTER the real mistake", text)
        self.assertNotIn("\x1b", text)  # nothing to paste into an editor but text

    def test_coloured_message_is_the_same_text(self):
        errors = xmreload.parse_errors(SCOPE.replace("5:26", "5:3"))
        plain = xmreload.render(errors, self.dir, "plain")
        ansi = xmreload.render(errors, self.dir, "ansi")
        self.assertIn("\x1b[", ansi)
        import re
        self.assertEqual(re.sub(r"\x1b\[[0-9;]*m", "", ansi), plain)

    def test_window_wraps_by_words_and_cuts_long_source_lines_but_the_clipboard_keeps_them(self):
        with open(os.path.join(self.dir, "xmonad.hs"), "w") as f:
            f.write("".join(f"line {n} " + "x" * 200 + "\n" for n in range(1, 11)))
        errors = xmreload.parse_errors(PARSE.replace("399:5", "5:5"))
        narrow = xmreload.render(errors, self.dir, "plain", width=80)
        for line in narrow.splitlines():
            self.assertLessEqual(len(line), 80, line)
        self.assertIn("…", narrow)
        # The hint is wrapped at spaces: every piece starts and ends on a whole word.
        hint = [l.strip() for l in narrow.splitlines() if l.strip() and "│" not in l][-4:]
        joined = " ".join(hint)
        self.assertIn("unclosed ( [ { or a missing comma or quote.", joined)
        whole = xmreload.render(errors, self.dir, "plain")  # no width: the clipboard version
        self.assertIn("x" * 200, whole)
        self.assertNotIn("…", whole)

    def test_plural_and_summary(self):
        errors = xmreload.parse_errors(MANY)
        self.assertIn("has 2 errors", xmreload.render(errors, self.dir, "plain"))
        self.assertEqual(xmreload.summary(errors), "xmonad.hs:5:26  Variable not in scope: termnal :: String (+1 more)")

    def test_hints(self):
        h = lambda m: xmreload.hint_for({"message": m})  # noqa: E731
        self.assertIn("unclosed", h("parse error on input ‘]’"))
        self.assertIn("misspelled", h("Variable not in scope: foo"))
        self.assertIn("types", h("Couldn't match type ‘Int’ with ‘String’"))
        self.assertIn("not installed", h("Could not find module ‘XMonad.Foo’"))
        self.assertEqual(h("something else"), "")


class Outside(unittest.TestCase):
    def test_recompile_reports_the_exit_code_and_the_output(self):
        rc, out, secs = xmreload.recompile(("sh", "-c", "echo to-out; echo to-err >&2; exit 3"))
        self.assertEqual(rc, 3)
        self.assertIn("to-out", out)
        self.assertIn("to-err", out)
        self.assertGreaterEqual(secs, 0)
        rc, out, _ = xmreload.recompile(("definitely-not-a-command-xyz",))
        self.assertEqual(rc, 127)

    def test_xmessage_is_replaced_by_a_do_nothing_stand_in(self):
        # xmonad opens an xmessage window when the compilation fails and keeps the pipes open.
        rc, out, _ = xmreload.recompile(("sh", "-c", "command -v xmessage; xmessage anything; echo returned"))
        self.assertEqual(rc, 0)
        path = out.splitlines()[0]
        self.assertIn("xmonad-reload-", path)
        self.assertIn("returned", out)
        self.assertFalse(os.path.exists(path), "the stand-in is removed afterwards")

    def test_clipboard_uses_the_first_tool_that_works(self):
        with tempfile.TemporaryDirectory() as bin_dir, tempfile.TemporaryDirectory() as out_dir:
            target = os.path.join(out_dir, "got")
            for name, body in (("xclip", "exit 1"), ("wl-copy", f"/bin/cat > {target}")):
                p = os.path.join(bin_dir, name)
                with open(p, "w") as f:
                    f.write(f"#!/bin/sh\n{body}\n")
                os.chmod(p, 0o755)
            with mock.patch.dict(os.environ, {"PATH": bin_dir}):
                self.assertTrue(xmreload.copy_to_clipboard("the error"))
            with open(target) as got:
                self.assertEqual(got.read(), "the error")
            with mock.patch.dict(os.environ, {"PATH": out_dir}):
                self.assertFalse(xmreload.copy_to_clipboard("x"))


@unittest.skipUnless(shutil.which("xmonad") and shutil.which("ghc"), "needs xmonad and ghc")
class RealCompile(unittest.TestCase):
    """Compile tiny configurations with the real xmonad; --dry never restarts or opens anything."""

    def run_script(self, source):
        with tempfile.TemporaryDirectory() as d:
            for sub in ("conf", "cache", "data"):
                os.mkdir(os.path.join(d, sub))
            with open(os.path.join(d, "conf", "xmonad.hs"), "w") as f:
                f.write(source)
            env = dict(os.environ, XMONAD_CONFIG_DIR=os.path.join(d, "conf"),
                       XMONAD_CACHE_DIR=os.path.join(d, "cache"), XMONAD_DATA_DIR=os.path.join(d, "data"))
            # A timeout, not a wait: a window waiting for a click would hang here.
            return subprocess.run([sys.executable, os.path.join(REPO, "bin", ".local", "bin", "xmonad-reload"),
                                   "--dry"], capture_output=True, text=True, env=env, timeout=120,
                                  stdin=subprocess.DEVNULL)

    def test_a_good_configuration_reloads(self):
        r = self.run_script("import XMonad\nmain :: IO ()\nmain = xmonad def\n")
        self.assertEqual((r.returncode, r.stdout.startswith("reloaded (")), (0, True), r.stderr)

    def test_a_syntax_error_is_reported_with_its_place_and_does_not_hang(self):
        r = self.run_script("import XMonad\nmain :: IO ()\nmain = xmonad def {\n  terminal = \"x\"\n")
        self.assertEqual(r.returncode, 1)
        self.assertIn("✗ xmonad configuration has 1 error", r.stdout)
        self.assertRegex(r.stdout, r"xmonad\.hs:\d+:\d+")
        self.assertIn("▶", r.stdout)

    def test_a_type_error_is_reported(self):
        r = self.run_script("import XMonad\nmain :: IO ()\nmain = xmonad def { terminal = 42 }\n")
        self.assertEqual(r.returncode, 1)
        self.assertIn("xmonad.hs:3:", r.stdout)
        self.assertIn("No instance for", r.stdout)  # 42 is not a String

    def test_no_xmessage_window_is_left_behind(self):
        before = subprocess.run(["pgrep", "-c", "-x", "xmessage"], capture_output=True, text=True).stdout.strip()
        self.run_script("import XMonad\nmain = xmonad def {\n")
        after = subprocess.run(["pgrep", "-c", "-x", "xmessage"], capture_output=True, text=True).stdout.strip()
        self.assertEqual(before, after)


if __name__ == "__main__":
    unittest.main()
