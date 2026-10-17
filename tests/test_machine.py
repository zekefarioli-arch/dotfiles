"""Tests for the `machine` tool (number and name checks). Pure logic, no network."""
import importlib.machinery
import importlib.util
import os
import sys
import unittest

sys.dont_write_bytecode = True
REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PATH = os.path.join(REPO, "bin", ".local", "bin", "machine")
loader = importlib.machinery.SourceFileLoader("machine_tool", PATH)
machine = importlib.util.module_from_spec(importlib.util.spec_from_loader("machine_tool", loader))
loader.exec_module(machine)

CONF = {"reserved": "3, 11,137,254", "pool": "50-200"}


class CheckNumber(unittest.TestCase):
    def test_free_number_outside_the_pool(self):
        self.assertEqual(machine.check_number("40", CONF), (40, None))
        self.assertEqual(machine.check_number("201", CONF), (201, None))

    def test_rejects_bad_input_and_edges(self):
        for text in ("0", "255", "300", "abc", "", "-4", "4.5"):
            self.assertIsNone(machine.check_number(text, CONF)[0], text)

    def test_rejects_reserved_and_pool(self):
        for text in ("3", "11", "137", "254", "50", "200", "120"):
            n, why = machine.check_number(text, CONF)
            self.assertIsNone(n, text)
            self.assertTrue(why)

    def test_without_config_only_the_range_applies(self):
        self.assertEqual(machine.check_number("3", {}), (3, None))
        self.assertIsNone(machine.check_number("0", {})[0])


class Names(unittest.TestCase):
    def test_valid(self):
        for name in ("zeke-ryzen9-desktop", "arch", "a1"):
            self.assertTrue(machine.valid_name(name), name)

    def test_invalid(self):
        for name in ("", "-x", "Zeke", "a b", "a_b", "x" * 64):
            self.assertFalse(machine.valid_name(name), name)


class ReadConf(unittest.TestCase):
    def test_missing_file_is_empty(self):
        self.assertEqual(machine.read_conf("/nonexistent/network.conf"), {})


if __name__ == "__main__":
    unittest.main()
