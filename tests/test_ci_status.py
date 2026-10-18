"""Tests for ci-status: the parsing and the text of the notification.

Run from the repo root:  python3 -m unittest discover -s tests -v
They use a sample of what the VM prints and a fake /proc; nothing is asked of the real VM.
"""
import os
import sys
import shutil
import subprocess
import tempfile
import unittest
from unittest import mock

sys.dont_write_bytecode = True
REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(REPO, "bin", ".local", "lib", "ci-status"))
import cistatus  # noqa: E402

MOCK = "actions.runner.zekefarioli-arch-mock-oclock.ryzen-ci-runner.service"
DUE = "actions.runner.zekefarioli-arch-due-card.ryzen-ci-runner-duecard.service"

SAMPLE = f"""unit={DUE}
ActiveState=active
MemoryCurrent=124530688
MemoryPeak=255217664
TasksCurrent=21
Anon=45170688
File=76038144
Workers=0
unit={MOCK}
ActiveState=active
MemoryCurrent=3221225472
MemoryPeak=4027715584
TasksCurrent=80
Anon=2147483648
File=1073741824
Workers=1
mem_total=6136553472
mem_used=2576980377
mem_avail=3000000000
mem_cache=3221225472
"""


class Parsing(unittest.TestCase):
    def test_runners_and_vm_memory(self):
        r = cistatus.parse_remote(SAMPLE)
        self.assertEqual([x["unit"] for x in r["runners"]], [DUE, MOCK])
        mock = r["runners"][1]
        self.assertEqual((mock["anon"], mock["file"], mock["workers"], mock["active"]),
                         (2147483648, 1073741824, 1, True))
        self.assertEqual((r["mem_total"], r["mem_used"]), (6136553472, 2576980377))

    def test_a_counter_systemd_does_not_have_is_unknown_not_a_crash(self):
        r = cistatus.parse_remote(f"unit={MOCK}\nActiveState=active\nMemoryCurrent=[not set]\nAnon=\nWorkers=0\n")
        self.assertIsNone(r["runners"][0]["current"])
        self.assertIsNone(r["runners"][0]["anon"])

    def test_repo_of_the_unit(self):
        self.assertEqual(cistatus.repo_of(MOCK, "zekefarioli-arch"), "mock-oclock")
        self.assertEqual(cistatus.repo_of(DUE, "zekefarioli-arch"), "due-card")
        self.assertIsNone(cistatus.repo_of(MOCK, "someone-else"))
        self.assertIsNone(cistatus.repo_of("sshd.service", "zekefarioli-arch"))

    def test_sizes(self):
        self.assertEqual(cistatus.fmt_bytes(70 * 1024 * 1024), "70 MiB")
        self.assertEqual(cistatus.fmt_bytes(1.5 * 1024 ** 3), "1.5 GiB")
        self.assertEqual(cistatus.fmt_bytes(None), "?")


class Report(unittest.TestCase):
    def report(self, **kw):
        remote = cistatus.parse_remote(SAMPLE)
        return cistatus.format_report(kw.get("remote", remote), kw.get("held", 5 * 1024 ** 3),
                                      kw.get("github", {"waiting": 1, "running": 0}), "zekefarioli-arch")

    def test_counts_and_one_line_per_runner(self):
        title, body = self.report()
        self.assertEqual(title, "CI: 2 runners · 1 working")
        lines = body.splitlines()
        self.assertEqual(lines[0], "○ due-card: idle · 43 MiB + 73 MiB cache (peak 243 MiB)")
        self.assertEqual(lines[1], "● mock-oclock: working · 2.0 GiB + 1.0 GiB cache (peak 3.8 GiB)")
        self.assertIn("VM: 2.4 GiB of 5.7 GiB used inside", lines)
        self.assertEqual(lines[-2], "GitHub: 1 waiting, 0 running")
        # The total is the last line, and it says what this computer pays.
        self.assertEqual(lines[-1], "Total on this computer: 5.0 GiB")

    def test_all_idle(self):
        remote = cistatus.parse_remote(SAMPLE.replace("Workers=1", "Workers=0"))
        title, body = self.report(remote=remote)
        self.assertEqual(title, "CI: 2 runners · 0 working")
        self.assertNotIn("●", body)

    def test_without_github(self):
        _, body = self.report(github=None)
        self.assertNotIn("GitHub", body)

    def test_the_total_is_last_and_coloured(self):
        remote = cistatus.parse_remote(SAMPLE)
        held, ram = 5 * 1024 ** 3, 14 * 1024 ** 3
        _, body = cistatus.format_report(remote, held, None, "zekefarioli-arch", ram, "pango")
        last = body.splitlines()[-1]
        self.assertEqual(last, '<span foreground="#f5c2e7"><b>Total on this computer: 5.0 GiB (36% of RAM)</b></span>')
        # Only the total is coloured.
        self.assertEqual(body.count("<span"), 1)
        _, body = cistatus.format_report(remote, held, None, "zekefarioli-arch", ram, "ansi")
        self.assertTrue(body.splitlines()[-1].startswith("\x1b[1;38;2;245;194;231mTotal on this computer"))
        self.assertTrue(body.splitlines()[-1].endswith("\x1b[0m"))
        _, body = cistatus.format_report(remote, held, None, "zekefarioli-arch", ram, None)
        self.assertEqual(body.splitlines()[-1], "Total on this computer: 5.0 GiB (36% of RAM)")

    def test_markup_is_escaped_in_the_notification(self):
        # A repository name with & or < must not become markup in dunst.
        unit = "actions.runner.o-r&x<y.name.service"
        remote = cistatus.parse_remote(f"unit={unit}\nActiveState=active\nAnon=1\nFile=1\nWorkers=0\n")
        _, body = cistatus.format_report(remote, 1024 ** 3, None, "o", None, "pango")
        self.assertIn("r&amp;x&lt;y", body)
        self.assertNotIn("r&x<y", body)
        # And in a terminal it is left as it is.
        _, body = cistatus.format_report(remote, 1024 ** 3, None, "o", None, None)
        self.assertIn("r&x<y", body)

    def test_vm_off_and_vm_not_answering(self):
        title, body = self.report(remote=None, held=None)
        self.assertEqual(title, "CI VM is off")
        title, body = self.report(remote=None, held=3 * 1024 ** 3)
        self.assertEqual(title, "CI VM: not reachable")
        self.assertIn("3.0 GiB", body)


GiB = 1024 ** 3
MiB = 1024 ** 2


def vm(workers=0, used=1.5 * GiB, cache=3 * GiB):
    sample = SAMPLE.replace("Workers=1", f"Workers={workers}")
    r = cistatus.parse_remote(sample)
    r["mem_used"], r["mem_cache"] = int(used), int(cache)
    return r


class TrimDecision(unittest.TestCase):
    """When it is right to ask the VM to give its memory back, and when it is not."""

    IDLE_GITHUB = {"waiting": 0, "running": 0}

    def test_idle_with_nothing_waiting_and_a_lot_held_goes(self):
        go, note = cistatus.trim_decision(vm(), int(5 * GiB), self.IDLE_GITHUB)
        self.assertTrue(go)
        self.assertIn("asking the VM", note)

    def test_a_working_runner_is_never_touched_and_says_nothing(self):
        self.assertEqual(cistatus.trim_decision(vm(workers=1), int(5 * GiB), self.IDLE_GITHUB), (False, None))

    def test_runs_waiting_on_github_mean_a_job_starts_soon(self):
        go, note = cistatus.trim_decision(vm(), int(5 * GiB), {"waiting": 3, "running": 0})
        self.assertFalse(go)
        self.assertIn("3 runs on GitHub", note)
        go, note = cistatus.trim_decision(vm(), int(5 * GiB), {"waiting": 0, "running": 1})
        self.assertFalse(go)
        self.assertIn("1 run on GitHub", note)

    def test_without_an_answer_from_github_it_does_not_guess(self):
        go, note = cistatus.trim_decision(vm(), int(5 * GiB), None)
        self.assertFalse(go)
        self.assertIn("GitHub did not answer", note)

    def test_already_tight_is_not_worth_it(self):
        go, note = cistatus.trim_decision(vm(used=1.5 * GiB), int(1.8 * GiB), self.IDLE_GITHUB)
        self.assertFalse(go)
        self.assertIn("Nothing to give back", note)
        # The edge: 512 MiB more than it uses is the smallest case that goes.
        self.assertTrue(cistatus.trim_decision(vm(used=1 * GiB), int(1.5 * GiB), self.IDLE_GITHUB)[0])
        self.assertFalse(cistatus.trim_decision(vm(used=1 * GiB), int(1.5 * GiB) - 1, self.IDLE_GITHUB)[0])

    def test_vm_not_readable_or_not_running(self):
        self.assertEqual(cistatus.trim_decision(None, int(5 * GiB), self.IDLE_GITHUB), (False, None))
        self.assertFalse(cistatus.trim_decision(vm(), None, self.IDLE_GITHUB)[0])


class TrimReport(unittest.TestCase):
    def test_it_says_how_much_came_back(self):
        before, after = vm(cache=3 * GiB), vm(used=1.4 * GiB, cache=0.7 * GiB)
        title, body = cistatus.format_trim_result(before, after, int(4.9 * GiB), int(3.2 * GiB), 14 * GiB, None)
        self.assertEqual(title, "CI VM: gave back 1.7 GiB")
        self.assertIn("Inside the VM: disk cache 3.0 GiB → 717 MiB, used 1.4 GiB", body)
        self.assertEqual(body.splitlines()[-1], "This computer: 4.9 GiB → 3.2 GiB (23% of RAM)")

    def test_when_little_comes_back_it_says_so(self):
        title, body = cistatus.format_trim_result(vm(), vm(), int(4.9 * GiB), int(4.88 * GiB), None, None)
        self.assertEqual(title, "CI VM: nothing came back")
        self.assertIn("could not free much, or a job just started", body)

    def test_the_last_line_is_coloured_for_the_notification(self):
        _, body = cistatus.format_trim_result(vm(), vm(), 5 * GiB, 3 * GiB, 14 * GiB, "pango")
        self.assertTrue(body.splitlines()[-1].startswith('<span foreground="#f5c2e7"><b>This computer:'))

    def test_the_note_goes_before_the_total_in_the_first_notification(self):
        _, body = cistatus.format_report(vm(), 5 * GiB, {"waiting": 0, "running": 0}, "zekefarioli-arch",
                                         14 * GiB, None, "a note & more")
        lines = body.splitlines()
        self.assertEqual(lines[-2], "a note & more")
        self.assertTrue(lines[-1].startswith("Total on this computer"))
        _, body = cistatus.format_report(vm(), 5 * GiB, None, None, None, "pango", "a note & more")
        self.assertIn("a note &amp; more", body)

    def test_the_vm_cache_is_read(self):
        self.assertEqual(cistatus.parse_remote(SAMPLE)["mem_cache"], 3221225472)


class Settling(unittest.TestCase):
    def test_waits_until_the_reading_stops_changing(self):
        readings = iter([5 * GiB, 4 * GiB, 3.2 * GiB, 3.19 * GiB, 3.0 * GiB])
        slept = []
        got = cistatus.settle_held(sleep=slept.append, read=lambda: next(readings))
        self.assertEqual(got, 3.19 * GiB)
        self.assertEqual(slept, [3.0, 3.0, 3.0])

    def test_gives_up_after_the_limit(self):
        n = iter(range(100))
        # Readings 10, 9, 8, 7 GiB at 0, 3, 6, 9 s: it never settles, and returns the last one read.
        got = cistatus.settle_held(sleep=lambda s: None, read=lambda: (10 - next(n)) * GiB, limit=9)
        self.assertEqual(got, 7 * GiB)

    def test_a_vm_that_stopped_has_nothing_to_wait_for(self):
        self.assertIsNone(cistatus.settle_held(sleep=lambda s: None, read=lambda: None))


class GitHubQueue(unittest.TestCase):
    """The queue count drives an action now, so what counts as "waiting" matters."""

    def counts(self, answers):
        outputs = iter(answers)
        with mock.patch.object(cistatus, "_gh", lambda args, timeout=8: next(outputs)):
            return cistatus.github_queue(["a", "b"][:len(answers)], "me")

    def test_every_state_before_a_job_runs_counts_as_waiting(self):
        runs = '[{"status":"requested"},{"status":"queued"},{"status":"waiting"},{"status":"pending"},' \
               '{"status":"in_progress"},{"status":"completed"}]'
        self.assertEqual(self.counts([runs]), {"waiting": 4, "running": 1})

    def test_two_repositories_add_up(self):
        self.assertEqual(self.counts(['[{"status":"queued"}]', '[{"status":"in_progress"},{"status":"queued"}]']),
                         {"waiting": 2, "running": 1})

    def test_an_answer_that_is_not_json_is_no_answer_not_a_crash(self):
        self.assertIsNone(self.counts(["<html>rate limited</html>"]))
        self.assertIsNone(self.counts(['{"message": "Bad credentials"}']))
        self.assertIsNone(self.counts(['[{"status":"queued"}]', None]))  # one repository did not answer


class TrimResultEdge(unittest.TestCase):
    def test_a_vm_that_vanished_is_not_reported_as_a_win(self):
        title, body = cistatus.format_trim_result(vm(), None, int(4.9 * GiB), None, 14 * GiB, None)
        self.assertEqual(title, "CI VM is off now")
        self.assertNotIn("gave back", title + body)


def load_script():
    """The ci-status script as a module (it has no .py name)."""
    import importlib.machinery
    import importlib.util
    path = os.path.join(REPO, "bin", ".local", "bin", "ci-status")
    spec = importlib.util.spec_from_loader("ci_status_script", importlib.machinery.SourceFileLoader("ci_status_script", path))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class Wiring(unittest.TestCase):
    """main(): what it shows, in which order, and when it asks the VM for anything."""

    def setUp(self):
        self.script = load_script()
        self.events = []
        lock_dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, lock_dir)
        env = mock.patch.dict(os.environ, {"CI_STATUS_LOCK": os.path.join(lock_dir, "lock")})
        env.start()
        self.addCleanup(env.stop)

        def notify(cmd, **kw):
            self.events.append(("notify", cmd[cmd.index("-t") + 1], cmd[-2], cmd[-1]))
            return mock.Mock(returncode=0)

        for target, new in (
            (subprocess, ("run", notify)),
            (cistatus, ("host_total_bytes", lambda: 14 * GiB)),
            (cistatus, ("settle_held", lambda: int(3.2 * GiB))),
            (cistatus, ("ask_vm", lambda t: vm(used=1.4 * GiB, cache=0.7 * GiB))),
        ):
            p = mock.patch.object(target, new[0], new[1])
            p.start()
            self.addCleanup(p.stop)
        self.trim_calls = []
        self.trim_ok = True

        def trim(target, timeout=40):
            self.events.append(("trim", target))
            return self.trim_ok

        p = mock.patch.object(cistatus, "trim_vm", trim)
        p.start()
        self.addCleanup(p.stop)

    def collect(self, workers=0, github=None, held=int(5 * GiB)):
        github = {"waiting": 0, "running": 0} if github is None else github
        p = mock.patch.object(cistatus, "collect", lambda: (vm(workers=workers), held, github, "zekefarioli-arch", "ci@vm"))
        p.start()
        self.addCleanup(p.stop)

    def kinds(self):
        return [e[0] for e in self.events]

    def test_idle_empty_queue_shows_then_trims_then_replaces_the_notification(self):
        self.collect()
        self.assertEqual(self.script.main([]), 0)
        self.assertEqual(self.kinds(), ["notify", "trim", "notify"])
        first, _, second = self.events
        self.assertEqual(first[1], "60000")                       # still on screen when replaced
        self.assertIn("asking the VM", first[3])
        self.assertEqual(second[1], "10000")
        self.assertEqual(second[2], "CI VM: gave back 1.8 GiB")   # 5.0 -> 3.2 GiB
        self.assertEqual(self.events[1], ("trim", "ci@vm"))

    def test_no_trim_only_looks(self):
        self.collect()
        self.assertEqual(self.script.main(["--no-trim"]), 0)
        self.assertEqual(self.kinds(), ["notify"])

    def test_a_queue_means_no_trim_and_says_why(self):
        self.collect(github={"waiting": 3, "running": 0})
        self.script.main([])
        self.assertEqual(self.kinds(), ["notify"])
        self.assertIn("3 runs on GitHub", self.events[0][3])

    def test_a_working_runner_means_no_trim(self):
        self.collect(workers=1)
        self.script.main([])
        self.assertEqual(self.kinds(), ["notify"])

    def test_a_failed_trim_is_reported_and_is_an_error(self):
        self.collect()
        self.trim_ok = False
        self.assertEqual(self.script.main([]), 1)
        self.assertEqual(self.kinds(), ["notify", "trim", "notify"])
        self.assertEqual(self.events[-1][2], "CI VM: could not give memory back")

    def test_a_second_run_while_one_is_trimming_does_not_start_another(self):
        self.collect()
        import fcntl
        held = open(os.environ["CI_STATUS_LOCK"], "w")
        fcntl.flock(held, fcntl.LOCK_EX)
        self.addCleanup(held.close)
        self.assertEqual(self.script.main([]), 0)
        self.assertEqual(self.kinds(), ["notify"])
        self.assertEqual(self.events[0][2], "CI VM: already giving memory back")

    def test_print_goes_to_the_terminal_not_to_notifications(self):
        self.collect()
        import io
        out = io.StringIO()
        with mock.patch("sys.stdout", out):
            self.script.main(["--print"])
        self.assertNotIn("notify", self.kinds())
        self.assertIn("CI VM: gave back", out.getvalue())


class FakeProc(unittest.TestCase):
    def make(self, root, pid, cmdline, rss_kb):
        d = os.path.join(root, str(pid))
        os.makedirs(d)
        with open(os.path.join(d, "cmdline"), "wb") as f:
            f.write(b"\0".join(a.encode() for a in cmdline) + b"\0")
        with open(os.path.join(d, "status"), "w") as f:
            f.write(f"Name:\tx\nVmRSS:\t{rss_kb} kB\n")

    def test_finds_the_vm_by_name_and_ignores_lookalikes(self):
        with tempfile.TemporaryDirectory() as root:
            self.make(root, 10, ["/usr/bin/qemu-system-x86_64", "-name", "guest=other,debug-threads=on"], 111)
            self.make(root, 11, ["/usr/bin/qemu-system-x86_64", "-name", "guest=ci-runner,debug-threads=on"], 5242880)
            self.make(root, 12, ["/bin/grep", "guest=ci-runner"], 222)
            os.makedirs(os.path.join(root, "self"))
            self.assertEqual(cistatus.qemu_held_bytes(root), 5242880 * 1024)

    def test_not_running(self):
        with tempfile.TemporaryDirectory() as root:
            self.make(root, 10, ["/usr/bin/qemu-system-x86_64", "-name", "guest=other"], 1)
            self.assertIsNone(cistatus.qemu_held_bytes(root))
            self.assertIsNone(cistatus.qemu_held_bytes(os.path.join(root, "missing")))


class HostRam(unittest.TestCase):
    def test_reads_memtotal(self):
        with tempfile.TemporaryDirectory() as root:
            with open(os.path.join(root, "meminfo"), "w") as f:
                f.write("MemTotal:       14680064 kB\nMemFree:  1 kB\n")
            self.assertEqual(cistatus.host_total_bytes(root), 14680064 * 1024)
            self.assertIsNone(cistatus.host_total_bytes(os.path.join(root, "missing")))


class Target(unittest.TestCase):
    def test_environment_wins_then_the_config_file(self):
        with tempfile.TemporaryDirectory() as home:
            self.assertIsNone(cistatus.vm_target(env={}, home=home))
            os.makedirs(os.path.join(home, ".config", "ci-status"))
            with open(os.path.join(home, ".config", "ci-status", "vm"), "w") as f:
                f.write("ci@10.0.0.5\n")
            self.assertEqual(cistatus.vm_target(env={}, home=home), "ci@10.0.0.5")
            self.assertEqual(cistatus.vm_target(env={"CI_VM": "x@y"}, home=home), "x@y")


if __name__ == "__main__":
    unittest.main()
