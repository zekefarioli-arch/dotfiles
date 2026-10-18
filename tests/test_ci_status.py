"""Tests for ci-status: the parsing and the text of the notification.

Run from the repo root:  python3 -m unittest discover -s tests -v
They use a sample of what the VM prints and a fake /proc; nothing is asked of the real VM.
"""
import os
import sys
import tempfile
import unittest

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
