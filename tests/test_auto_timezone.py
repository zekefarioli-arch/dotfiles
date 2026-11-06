"""Tests for the dispatcher script 90-auto-timezone: it must not follow a Proton VPN's country.

Run from the repo root:  python3 -m unittest discover -s tests -v
curl, nmcli, timedatectl and logger are fakes in a temporary directory; nothing real is touched.
"""
import os
import stat
import subprocess
import tempfile
import unittest

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SCRIPT = os.path.join(REPO, "system", "etc", "NetworkManager", "dispatcher.d", "90-auto-timezone")


def run(iface, action, vpn_active, geoip_tz="America/Argentina/Buenos_Aires", current="Europe/London"):
    """Run the script with fake tools; return the timezones it tried to set."""
    with tempfile.TemporaryDirectory() as d:
        calls = os.path.join(d, "calls")

        def fake(name, body):
            path = os.path.join(d, name)
            with open(path, "w") as f:
                f.write("#!/bin/bash\n" + body + "\n")
            os.chmod(path, os.stat(path).st_mode | stat.S_IXUSR)

        fake("curl", f"echo '{{\"time_zone\": \"{geoip_tz}\"}}'")
        fake("nmcli", "echo 'pvpn-ar73:wireguard'" if vpn_active else "echo 'Wired connection 1:802-3-ethernet'")
        fake("timedatectl", f'if [ "$1" = show ]; then echo {current}; else echo "$2" >> {calls}; fi')
        fake("logger", "true")
        env = dict(os.environ, PATH=d + ":" + os.environ["PATH"])
        subprocess.run(["bash", SCRIPT, iface, action], env=env, check=True)
        return open(calls).read().split() if os.path.exists(calls) else []


class AutoTimezone(unittest.TestCase):
    def test_changes_the_zone_when_the_network_moved(self):
        self.assertEqual(run("eno1", "up", vpn_active=False), ["America/Argentina/Buenos_Aires"])

    def test_ignores_the_vpn_interface_itself(self):
        self.assertEqual(run("pvpn-ar73", "up", vpn_active=True), [])

    def test_ignores_connectivity_changes_while_the_vpn_is_up(self):
        self.assertEqual(run("", "connectivity-change", vpn_active=True), [])

    def test_ignores_other_actions(self):
        self.assertEqual(run("eno1", "down", vpn_active=False), [])

    def test_no_change_when_the_zone_is_already_right(self):
        self.assertEqual(run("eno1", "up", vpn_active=False, geoip_tz="Europe/London"), [])


if __name__ == "__main__":
    unittest.main()
