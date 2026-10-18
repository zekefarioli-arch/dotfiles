"""Tests for system/ci-vm/ci-trim-memory.sh, the script that runs inside the CI VM.

Run from the repo root:  python3 -m unittest discover -s tests -v
The real script runs here against fake /proc files and stand-ins for pgrep, sync, logger and
sleep, so every branch is checked without root and without touching this machine's memory.
"""
import os
import stat
import subprocess
import tempfile
import unittest

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SCRIPT = os.path.join(REPO, "system", "ci-vm", "ci-trim-memory.sh")

MEMINFO = "MemTotal: 5992448 kB\nMemFree: {free} kB\nCached: {cached} kB\n"


class Trim(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()
        self.addCleanup(subprocess.run, ["rm", "-rf", self.dir])
        for sub in ("bin", "vm"):
            os.mkdir(os.path.join(self.dir, sub))
        for name in ("drop_caches", "compact_memory"):
            with open(os.path.join(self.dir, "vm", name), "w"):
                pass
        self.log = os.path.join(self.dir, "log")
        self.calls = os.path.join(self.dir, "calls")
        self.stub("logger", f'shift 2; echo "$*" >> {self.log}')          # logger -t TAG -- MESSAGE
        self.stub("sync", f"echo sync >> {self.calls}")
        self.stub("sleep", f"echo sleep >> {self.calls}")

    def stub(self, name, body):
        path = os.path.join(self.dir, "bin", name)
        with open(path, "w") as f:
            f.write(f"#!/bin/sh\n{body}\n")
        os.chmod(path, os.stat(path).st_mode | stat.S_IEXEC)

    def run_script(self, *args, job=False, cached_mib=2400, free_mib=2100, load1="0.05",
                   loadavg=None, pgrep_exit=None):
        # pgrep: 0 a process matched, 1 none, anything else it could not tell.
        self.stub("pgrep", f"exit {pgrep_exit if pgrep_exit is not None else (0 if job else 1)}")
        with open(os.path.join(self.dir, "meminfo"), "w") as f:
            f.write(MEMINFO.format(free=free_mib * 1024, cached=cached_mib * 1024))
        with open(os.path.join(self.dir, "loadavg"), "w") as f:
            f.write(loadavg or f"{load1} 0.10 0.10 1/200 1234\n")
        env = dict(os.environ, PATH=os.path.join(self.dir, "bin") + ":/usr/bin:/bin",
                   CI_TRIM_PROC_VM=os.path.join(self.dir, "vm"),
                   CI_TRIM_MEMINFO=os.path.join(self.dir, "meminfo"),
                   CI_TRIM_LOADAVG=os.path.join(self.dir, "loadavg"))
        r = subprocess.run(["sh", SCRIPT, *args], env=env, capture_output=True, text=True)
        self.assertEqual(r.returncode, 0, r.stderr)
        return r

    def read(self, name):
        path = os.path.join(self.dir, name)
        if not os.path.exists(path):
            return ""
        with open(path) as f:
            return f.read()

    def vm(self, name):
        return self.read(os.path.join("vm", name)).strip()

    def test_idle_with_cache_trims(self):
        self.run_script()
        self.assertEqual((self.vm("drop_caches"), self.vm("compact_memory")), ("3", "1"))
        self.assertIn("trimmed: cache 2400 -> 2400 MiB, free 2100 -> 2100 MiB", self.read("log"))
        # The data is written to disk before the cache is dropped, and the host gets a moment.
        self.assertEqual(self.read("calls").split(), ["sync", "sleep"])

    def test_a_running_job_is_never_touched_not_even_with_now(self):
        for args in ((), ("--now",)):
            self.run_script(*args, job=True)
            self.assertEqual((self.vm("drop_caches"), self.vm("compact_memory")), ("", ""))
        # It runs every minute, so skipping is silent: no log line and no command ran.
        self.assertEqual((self.read("log"), self.read("calls")), ("", ""))

    def test_busy_lately_waits(self):
        self.run_script(load1="1.50")
        self.assertEqual((self.vm("drop_caches"), self.read("log"), self.read("calls")), ("", "", ""))

    def test_only_the_load_of_the_last_minute_counts(self):
        # A job ended minutes ago: the 5 and 15-minute averages are still high, the 1-minute one is not.
        self.run_script(loadavg="0.05 1.40 1.10 1/200 1234\n")
        self.assertEqual(self.vm("drop_caches"), "3")

    def test_not_being_able_to_look_for_a_job_is_not_the_same_as_there_being_none(self):
        # pgrep exits 1 for "no process"; 2 or 3 (could not look) and 127 (not installed) mean "unknown".
        for code in (2, 3, 127):
            for args in ((), ("--now",)):
                self.run_script(*args, pgrep_exit=code)
                self.assertEqual((self.vm("drop_caches"), self.vm("compact_memory")), ("", ""), (code, args))
        self.assertEqual((self.read("log"), self.read("calls")), ("", ""))

    def test_the_load_limit_is_a_threshold_not_zero(self):
        self.run_script(load1="0.29")
        self.assertEqual(self.vm("drop_caches"), "3")
        self.setUp()
        self.run_script(load1="0.30")
        self.assertEqual(self.vm("drop_caches"), "")

    def test_nothing_to_give_back_is_silent(self):
        r = self.run_script(cached_mib=120)
        self.assertEqual((self.vm("drop_caches"), self.read("log"), self.read("calls")), ("", "", ""))
        self.assertEqual(r.stdout + r.stderr, "")

    def test_now_ignores_load_and_cache_size(self):
        self.run_script("--now", load1="3.00", cached_mib=50)
        self.assertEqual((self.vm("drop_caches"), self.vm("compact_memory")), ("3", "1"))
        self.assertIn("trimmed: cache 50 ->", self.read("log"))


if __name__ == "__main__":
    unittest.main()
