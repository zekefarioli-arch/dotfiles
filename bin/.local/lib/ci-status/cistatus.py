"""What the GitHub Actions runners of the CI virtual machine are doing and how much
memory each one takes (used by bin/ci-status, which shows it in a notification).

Where the numbers come from, and why:
- Each runner is a systemd service inside the VM (`actions.runner.<owner>-<repo>.<name>`),
  so its cgroup holds the runner and every job it started. One ssh call reads, per service,
  the memory of its processes (`anon`), the file cache charged to it (`file`, which the kernel
  gives back when it is needed), the peak, and whether a `Runner.Worker` (a job) is running.
- The memory the VM holds on this computer is the resident size of its qemu process, read from
  /proc: it is what the host really lost, and it can stay high after a job (see DECISIONS.md).
- GitHub (`gh`) only adds how many runs are waiting or running; it is optional.
- When nothing is working or waiting and the VM holds much more than it uses, ci-status also
  asks the VM to give the memory back (`ci-trim-memory.sh --now`, installed inside the VM from
  system/ci-vm/) and reports how much came back. That is the only thing here that changes
  anything, and only under those conditions; `--no-trim` makes it read-only.
"""
import concurrent.futures as cf
import html
import json
import os
import re
import subprocess
import time

# Run on the VM through `ssh ... sh -s`; prints key=value lines, one block per runner.
REMOTE_SCRIPT = r"""
for u in $(systemctl list-units --type=service --no-legend 'actions.runner.*' | awk '{print $1}'); do
  echo "unit=$u"
  cg=$(systemctl show "$u" -p ControlGroup --value)
  systemctl show "$u" -p ActiveState -p MemoryCurrent -p MemoryPeak -p TasksCurrent
  echo "Anon=$(awk '$1=="anon" {print $2}' /sys/fs/cgroup$cg/memory.stat 2>/dev/null)"
  echo "File=$(awk '$1=="file" {print $2}' /sys/fs/cgroup$cg/memory.stat 2>/dev/null)"
  echo "Workers=$(for p in $(cat /sys/fs/cgroup$cg/cgroup.procs 2>/dev/null); do cat /proc/$p/comm 2>/dev/null; done | grep -c '^Runner.Worker')"
done
free -b | awk '/Mem:/ {print "mem_total="$2; print "mem_used="$3; print "mem_avail="$7; print "mem_cache="$6}'
"""

GiB = 1 << 30
MiB = 1 << 20


def fmt_bytes(n):
    """1.3 GiB, 70 MiB. Unknown values show as '?'."""
    if n is None:
        return "?"
    if n >= GiB:
        return f"{n / GiB:.1f} GiB"
    return f"{round(n / MiB)} MiB"


def _int(value):
    try:
        return int(value)
    except (TypeError, ValueError):
        return None  # systemd prints "[not set]" when a counter is not available


def parse_remote(text):
    """The output of REMOTE_SCRIPT as {"runners": [...], "mem_total", "mem_used", "mem_avail", "mem_cache"}."""
    runners, vm, cur = [], {}, None
    for line in text.splitlines():
        if "=" not in line:
            continue
        key, value = line.strip().split("=", 1)
        if key == "unit":
            cur = {"unit": value}
            runners.append(cur)
        elif key in ("mem_total", "mem_used", "mem_avail", "mem_cache"):
            vm[key] = _int(value)
        elif cur is not None:
            cur[key] = value
    out = []
    for r in runners:
        out.append({
            "unit": r["unit"],
            "active": r.get("ActiveState") == "active",
            "current": _int(r.get("MemoryCurrent")),
            "peak": _int(r.get("MemoryPeak")),
            "anon": _int(r.get("Anon")),
            "file": _int(r.get("File")),
            "workers": _int(r.get("Workers")) or 0,
        })
    return {"runners": out, **vm}


def repo_of(unit, owner):
    """`actions.runner.zekefarioli-arch-mock-oclock.ryzen-ci-runner.service` -> `mock-oclock`."""
    m = re.match(r"actions\.runner\.(.+?)\.[^.]+\.service$", unit)
    if not m or not owner or not m.group(1).startswith(owner + "-"):
        return None
    return m.group(1)[len(owner) + 1:]


def qemu_held_bytes(proc="/proc", guest="ci-runner"):
    """Resident memory of the VM's qemu process on this computer, or None if it is not running."""
    name = f"guest={guest}".encode()
    try:
        pids = [p for p in os.listdir(proc) if p.isdigit()]
    except OSError:
        return None
    for pid in pids:
        try:
            with open(f"{proc}/{pid}/cmdline", "rb") as f:
                args = f.read().split(b"\0")
            # `qemu-system-x86_64 -name guest=ci-runner,debug-threads=on ...`
            if b"qemu" not in args[0] or not any(a == name or a.startswith(name + b",") for a in args):
                continue
            with open(f"{proc}/{pid}/status") as f:
                for line in f:
                    if line.startswith("VmRSS:"):
                        return int(line.split()[1]) * 1024
        except OSError:
            continue
    return None


ACCENT = "f5c2e7"  # the pink accent of the theme (Catppuccin Mocha)


def paint(text, style):
    """The total line in the accent colour: Pango markup for the notification, ANSI for a terminal."""
    if style == "pango":
        return f'<span foreground="#{ACCENT}"><b>{html.escape(text, quote=False)}</b></span>'
    if style == "ansi":
        r, g, b = (int(ACCENT[i:i + 2], 16) for i in (0, 2, 4))
        return f"\x1b[1;38;2;{r};{g};{b}m{text}\x1b[0m"
    return text


def host_total_bytes(proc="/proc"):
    """RAM of this computer, from /proc/meminfo (None if it cannot be read)."""
    try:
        with open(f"{proc}/meminfo") as f:
            for line in f:
                if line.startswith("MemTotal:"):
                    return int(line.split()[1]) * 1024
    except (OSError, ValueError):
        pass
    return None


def format_report(remote, held, github, owner=None, host_total=None, style=None, note=None):
    """(title, body) of the notification.

    remote: parse_remote() result, or None if the VM could not be asked.
    held: bytes the VM's qemu holds on this computer, or None if it is not running.
    github: {"waiting": n, "running": n} or None.
    host_total: RAM of this computer, to show the total as a share of it.
    style: None, "pango" or "ansi": how the last line (the total) is coloured.
    note: one more line before the total (what was or was not done about the memory).
    """
    lines = []
    if remote is None:
        if held is None:
            return "CI VM is off", "The virtual machine is not running, so no runner is."
        lines.append(f"The VM runs and holds {fmt_bytes(held)} on this computer,")
        lines.append("but ssh to it did not answer: the runners cannot be read.")
        return "CI VM: not reachable", "\n".join(lines)

    runners = sorted(remote["runners"], key=lambda r: r["unit"])
    working = sum(1 for r in runners if r["workers"] > 0)
    online = sum(1 for r in runners if r["active"])
    for r in runners:
        name = repo_of(r["unit"], owner) or r["unit"].split(".")[2]
        state = ("working" if r["workers"] > 0 else "idle") if r["active"] else "stopped"
        mark = "●" if r["workers"] > 0 else "○"
        line = f"{mark} {name}: {state} · {fmt_bytes(r['anon'])} + {fmt_bytes(r['file'])} cache"
        if r["peak"]:
            line += f" (peak {fmt_bytes(r['peak'])})"
        lines.append(line)
    lines.append(f"VM: {fmt_bytes(remote.get('mem_used'))} of {fmt_bytes(remote.get('mem_total'))} used inside")
    if github is not None:
        lines.append(f"GitHub: {github['waiting']} waiting, {github['running']} running")
    if note:
        lines.append(note)
    if style == "pango":
        lines = [html.escape(x, quote=False) for x in lines]
    # Last, in colour: what all of this costs this computer, which is what the VM holds (it can
    # be much more than the runners use inside, see DECISIONS.md).
    share = f" ({round(100 * held / host_total)}% of RAM)" if held and host_total else ""
    lines.append(paint(f"Total on this computer: {fmt_bytes(held)}{share}", style))
    title = f"CI: {online} runner{'s' if online != 1 else ''} · {working} working"
    return title, "\n".join(lines)


MIN_GIVE_BACK = 512 * MiB  # below this the VM is already tight: not worth a trim


def trim_decision(remote, held, github):
    """(go, note): whether to ask the VM to give memory back, and one line saying why not.

    Only when the VM could be read, no runner is working, GitHub says nothing is waiting or
    running (a job is about to start otherwise, and it would find the cache empty), and the
    VM holds clearly more than it uses.
    """
    if remote is None:
        return False, None
    if any(r["workers"] > 0 for r in remote["runners"]):
        return False, None  # busy: the memory is in use, nothing to say
    if github is None:
        return False, "GitHub did not answer, so memory is left alone (a job may be about to start)"
    pending = github["waiting"] + github["running"]
    if pending:
        return False, f"{pending} run{'s' if pending != 1 else ''} on GitHub: memory is left alone, a job starts soon"
    spare = (held or 0) - (remote.get("mem_used") or 0)
    if held is None or spare < MIN_GIVE_BACK:
        return False, f"Nothing to give back: the VM holds only {fmt_bytes(max(spare, 0))} more than it uses"
    return True, "Nothing running or waiting: asking the VM to give its unused memory back..."


def format_trim_result(before, after, held_before, held_after, host_total=None, style=None):
    """(title, body) for the notification that replaces the first one after a trim.

    before, after: parse_remote() results taken before and after (either may be None).
    held_before, held_after: bytes the VM held on this computer.
    """
    if held_after is None:
        # The VM stopped between the trim and the reading: there is no "after" to compare with.
        return "CI VM is off now", paint(f"It held {fmt_bytes(held_before)} on this computer before.", style)
    freed = (held_before or 0) - held_after
    share = f" ({round(100 * held_after / host_total)}% of RAM)" if held_after and host_total else ""
    if freed >= 64 * MiB:
        title = f"CI VM: gave back {fmt_bytes(freed)}"
    else:
        title = "CI VM: nothing came back"
    lines = []
    if before and after:
        lines.append(f"Inside the VM: disk cache {fmt_bytes(before.get('mem_cache'))} → "
                     f"{fmt_bytes(after.get('mem_cache'))}, used {fmt_bytes(after.get('mem_used'))}")
    if freed < 64 * MiB:
        lines.append("The VM could not free much, or a job just started and used it.")
    if style == "pango":
        lines = [html.escape(x, quote=False) for x in lines]
    lines.append(paint(f"This computer: {fmt_bytes(held_before)} → {fmt_bytes(held_after)}{share}", style))
    return title, "\n".join(lines)


# ---- collectors (they touch the outside world; tests do not call them) ----

def vm_target(env=os.environ, home=os.path.expanduser("~")):
    """`user@address` of the VM: CI_VM, or the first line of ~/.config/ci-status/vm."""
    if env.get("CI_VM"):
        return env["CI_VM"]
    try:
        with open(os.path.join(home, ".config", "ci-status", "vm")) as f:
            line = f.readline().strip()
            return line or None
    except OSError:
        return None


def discover_vm_target(domain="ci-runner", user="ci"):
    """`user@address` from libvirt's DHCP lease, when the address in the config is out of date.
    Needs the user to be in the libvirt group (log in again after being added)."""
    try:
        out = subprocess.run(["virsh", "-c", "qemu:///system", "domifaddr", domain, "--source", "lease"],
                             capture_output=True, text=True, timeout=5).stdout
    except (OSError, subprocess.TimeoutExpired):
        return None
    m = re.search(r"\bipv4\s+(\d+\.\d+\.\d+\.\d+)/", out)
    return f"{user}@{m.group(1)}" if m else None


def ask_vm(target, timeout=8):
    """Run REMOTE_SCRIPT on the VM; the parsed result, or None if it did not answer."""
    if not target:
        return None
    try:
        out = subprocess.run(
            ["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=4", target, "sh", "-s"],
            input=REMOTE_SCRIPT, capture_output=True, text=True, timeout=timeout)
    except (OSError, subprocess.TimeoutExpired):
        return None
    if out.returncode != 0 or "unit=" not in out.stdout:
        return None
    return parse_remote(out.stdout)


def _gh(args, timeout=8):
    try:
        r = subprocess.run(["gh", *args], capture_output=True, text=True, timeout=timeout)
    except (OSError, subprocess.TimeoutExpired):
        return None
    return r.stdout if r.returncode == 0 else None


def github_queue(repos, owner):
    """Runs waiting and running over `repos`, or None if gh cannot tell."""
    if not repos or not owner:
        return None

    def one(repo):
        out = _gh(["run", "list", "-R", f"{owner}/{repo}", "--limit", "30", "--json", "status"])
        if out is None:
            return None
        try:
            statuses = [x.get("status") for x in json.loads(out)]
        except (ValueError, AttributeError):
            return None  # not the JSON gh promised: "GitHub did not answer", not a crash
        # `requested` is a run just triggered whose jobs are not queued yet: a job is about to start.
        return (sum(s in ("requested", "queued", "waiting", "pending") for s in statuses),
                sum(s == "in_progress" for s in statuses))

    with cf.ThreadPoolExecutor() as ex:
        results = list(ex.map(one, repos))
    if any(r is None for r in results):
        return None
    return {"waiting": sum(r[0] for r in results), "running": sum(r[1] for r in results)}


def gh_owner():
    out = _gh(["api", "user", "--jq", ".login"], timeout=5)
    return out.strip() if out else None


def _ask_with_fallback():
    """(remote, target): the answer of the VM and the `user@address` that gave it."""
    target = vm_target()
    remote = ask_vm(target)
    if remote is None:
        found = discover_vm_target()
        if found and found != target:
            target, remote = found, ask_vm(found)
    return remote, target


def trim_vm(target, timeout=40):
    """Ask the VM to give its unused memory back. True if its script ran without error."""
    try:
        r = subprocess.run(["ssh", "-o", "BatchMode=yes", "-o", "ConnectTimeout=4", target,
                            "sudo", "-n", "/usr/local/sbin/ci-trim-memory.sh", "--now"],
                           capture_output=True, text=True, timeout=timeout, stdin=subprocess.DEVNULL)
    except (OSError, subprocess.TimeoutExpired):
        return False
    return r.returncode == 0


def settle_held(sleep=time.sleep, read=None, step=3.0, limit=36.0, quiet=32 * MiB):
    """What the VM holds on this computer once it stops changing: the host is told in batches,
    for some seconds after the guest frees memory. Reads every `step` s until two readings differ
    by less than `quiet` bytes, or `limit` s have passed."""
    read = read or qemu_held_bytes
    last, waited = read(), 0.0
    while waited < limit:
        sleep(step)
        waited += step
        now = read()
        if now is None or last is None or abs(now - last) < quiet:
            return now
        last = now
    return last


def collect():
    """Everything the report needs, asking the VM and GitHub at the same time.
    Returns (remote, held, github, owner, target)."""
    with cf.ThreadPoolExecutor() as ex:
        f_vm = ex.submit(_ask_with_fallback)
        f_owner = ex.submit(gh_owner)
        (remote, target), owner = f_vm.result(), f_owner.result()
    github = None
    if remote and owner:
        repos = [r for r in (repo_of(x["unit"], owner) for x in remote["runners"]) if r]
        github = github_queue(repos, owner)
    return remote, qemu_held_bytes(), github, owner, target
