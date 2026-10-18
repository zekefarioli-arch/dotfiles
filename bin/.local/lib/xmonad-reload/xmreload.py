"""Recompile the xmonad configuration, and restart xmonad only if it compiled.

The old shortcut ran `xmonad --recompile; xmonad --restart`: with a mistake in xmonad.hs the
compilation failed, the restart still happened with the old binary, and nothing told you. This
module is what bin/xmonad-reload uses to do it properly:
- parse_errors / render turn GHC's output into a readable message: where, what, a few lines of
  the real source around it, and a hint for the usual causes;
- the plain text of that message goes to the clipboard, the coloured one to a terminal window
  (text there can be selected and copied), a short one to a notification.
Everything that touches the outside world is in the script; the functions here are pure, so
tests/test_xmonad_reload.py can check them with real GHC output.
"""
import os
import re
import textwrap
import subprocess
import tempfile
import time

# Catppuccin Mocha, as the rest of the desktop.
RED, PINK, BLUE, GREY, YELLOW, TEXT = "f38ba8", "f5c2e7", "89b4fa", "6c7086", "f9e2af", "cdd6f4"

# `xmonad.hs:399:5: error: [GHC-58481] parse error on input ‘]’`
#  or, for a span, `xmonad.hs:(12,5)-(14,9): error: [GHC-...] ...`
HEADER = re.compile(
    r"^(?P<file>[^\s:][^:]*?\.hs):(?:(?P<line>\d+):(?P<col>\d+)(?:-\d+)?|"
    r"\((?P<l2>\d+),(?P<c2>\d+)\)-\(\d+,\d+\)): (?P<kind>error|warning):?"
    r"(?: \[(?P<code>[^\]]+)\])?\s*(?P<msg>.*)$")
# The code excerpt GHC prints (`399 |     ]` and `    |     ^`): we print our own, with more lines.
EXCERPT = re.compile(r"^\s*\d*\s*\|")
TRAILER = "Please check the file for errors."


def parse_errors(output):
    """The errors in GHC's output: [{file, line, col, code, message, detail}], in order.
    Warnings are ignored: they do not stop the configuration from compiling."""
    errors, cur = [], None
    for raw in output.splitlines():
        m = HEADER.match(raw)
        if m:
            cur = None
            if m.group("kind") == "error":
                line = m.group("line") or m.group("l2")
                col = m.group("col") or m.group("c2")
                cur = {"file": m.group("file"), "line": int(line), "col": int(col),
                       "code": m.group("code") or "", "message": m.group("msg").strip(), "detail": []}
                errors.append(cur)
            continue
        if cur is None or raw.strip() == TRAILER or EXCERPT.match(raw):
            continue
        cur["detail"].append(raw.rstrip())
    for e in errors:
        d = e["detail"]
        while d and not d[0].strip():
            d.pop(0)
        while d and not d[-1].strip():
            d.pop()
        if not e["message"] and d:  # GHC sometimes starts the message on the next line
            e["message"] = d.pop(0).strip()
    return errors


def source_context(path, line, before=4, after=1):
    """[(number, text)] of the lines around `line` in `path`; [] if it cannot be read."""
    try:
        with open(path, encoding="utf-8", errors="replace") as f:
            lines = f.read().splitlines()
    except OSError:
        return []
    start = max(1, line - before)
    ctx = [(n, lines[n - 1]) for n in range(start, min(len(lines), line + after) + 1)]
    if line > len(lines):
        # "unexpected end of file": the line GHC points at does not exist, it is the end.
        ctx.append((line, "⟨end of the file⟩"))
    return ctx


def hint_for(error):
    """One sentence about the usual cause, or ''. GHC often reports a parse error far below the
    real mistake, which is the thing people lose time on."""
    msg = error["message"].lower()
    if msg.startswith("parse error") or "unexpected" in msg:
        return ("A parse error is often reported lines AFTER the real mistake: look above for an "
                "unclosed ( [ { or a missing comma or quote.")
    if "not in scope" in msg:
        return "A name is misspelled, or the import that provides it is missing."
    if "couldn't match" in msg or "no instance" in msg:
        return "The types do not fit: check the arguments of the function named in the message."
    if "could not find module" in msg:
        return "The module is not installed or the name of the import is wrong."
    return ""


def _paint(text, rgb, style, bold=False):
    if style != "ansi":
        return text
    r, g, b = (int(rgb[i:i + 2], 16) for i in (0, 2, 4))
    return f"\x1b[{'1;' if bold else ''}38;2;{r};{g};{b}m{text}\x1b[0m"


def render(errors, config_dir, style="plain", width=None):
    """The message for a window (style "ansi") or the clipboard (style "plain").

    With `width`, texts are wrapped at word boundaries and long lines of source are cut with "…"
    so nothing wraps in the middle of a word in a window. The clipboard version has no width: it
    keeps every line whole, which is what you want to paste.
    """
    def wrapped(text):
        if not width or not text.strip():
            return [text]
        return textwrap.wrap(text, width=max(20, width - 4), break_long_words=False,
                             break_on_hyphens=False) or [text]

    n = len(errors)
    out = [_paint(f"✗ xmonad configuration has {n} error{'s' if n != 1 else ''}", RED, style, True),
           _paint("The running xmonad was NOT restarted: nothing has changed.", GREY, style), ""]
    for i, e in enumerate(errors, 1):
        where = f"{e['file']}:{e['line']}:{e['col']}"
        code = f"  [{e['code']}]" if e["code"] else ""
        out.append(_paint(f"{i}. {where}", BLUE, style, True) + _paint(code, GREY, style))
        for part in wrapped(e["message"]):
            out.append("   " + _paint(part, PINK, style, True))
        for d in e["detail"]:
            if not d.strip():
                out.append("")
                continue
            for part in wrapped(d.strip()):
                out.append("   " + _paint(part, TEXT, style))
        ctx = source_context(os.path.join(config_dir, e["file"]), e["line"])
        if ctx:
            out.append("")
            room = width - 14 if width else None  # what is left of a line after "   ▶  399 │ "
            for num, text in ctx:
                if room and len(text) > room:
                    text = text[:room - 1] + "…"
                here = num == e["line"]
                mark = "▶" if here else " "
                number = _paint(f"{mark} {num:>4} │", RED if here else GREY, style, here)
                out.append(f"   {number} {_paint(text, TEXT, style) if here else text}")
        hint = hint_for(e)
        if hint:
            out.append("")
            for part in wrapped("hint: " + hint):
                out.append("   " + _paint(part, YELLOW, style))
        out.append("")
    return "\n".join(out).rstrip("\n") + "\n"


def summary(errors):
    """Short text for the notification."""
    first = errors[0]
    more = f" (+{len(errors) - 1} more)" if len(errors) > 1 else ""
    return f"{first['file']}:{first['line']}:{first['col']}  {first['message']}{more}"


def raw_failure(output):
    """When the compilation failed but GHC printed no error we recognise: the output, without
    the long command line xmonad prints first."""
    keep = [l for l in output.splitlines()
            if not l.startswith(("XMonad will use ghc", "XMonad recompiling", "$ ghc",
                                 "Errors detected while compiling"))]
    return "\n".join(keep).strip() or "xmonad --recompile failed without a message."


def recompile(cmd=("xmonad", "--recompile"), env=None):
    """(returncode, output, seconds). Output is stdout and stderr together.

    When the compilation fails, xmonad forks an `xmessage` window with the error and leaves it
    there (it also keeps our pipes open, so we would wait for someone to click it). A stand-in
    `xmessage` that does nothing goes first in PATH for this call: the error is on stderr anyway,
    and we show it ourselves.
    """
    t = time.monotonic()
    with tempfile.TemporaryDirectory(prefix="xmonad-reload-") as stub:
        fake = os.path.join(stub, "xmessage")
        with open(fake, "w") as f:
            f.write("#!/bin/sh\nexit 0\n")
        os.chmod(fake, 0o755)
        env = dict(os.environ if env is None else env)
        env["PATH"] = stub + os.pathsep + env.get("PATH", "")
        try:
            r = subprocess.run(list(cmd), capture_output=True, text=True, env=env, timeout=300,
                               stdin=subprocess.DEVNULL)
            return r.returncode, (r.stdout or "") + (r.stderr or ""), time.monotonic() - t
        except FileNotFoundError:
            return 127, f"{cmd[0]}: command not found", time.monotonic() - t
        except subprocess.TimeoutExpired:
            return 124, "xmonad --recompile took more than 5 minutes and was stopped.", time.monotonic() - t


def config_dir(env=os.environ):
    return env.get("XMONAD_CONFIG_DIR") or os.path.expanduser("~/.xmonad")


def copy_to_clipboard(text):
    """True if some tool took it: xclip, else wl-copy, else CopyQ."""
    for cmd in (["xclip", "-selection", "clipboard"], ["wl-copy"], ["copyq", "add", "-"]):
        try:
            r = subprocess.run(cmd, input=text, text=True, capture_output=True, timeout=5)
        except (OSError, subprocess.TimeoutExpired):
            continue
        if r.returncode == 0:
            return True
    return False
