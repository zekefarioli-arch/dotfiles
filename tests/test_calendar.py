"""Tests for the mini calendar's event handling (calevents).

Run from the repo root:  python3 -m unittest discover -s tests -v
They use a local sample iCal file and temporary directories only.
"""
import datetime as dt
import os
import sys
import tempfile
import unittest

sys.dont_write_bytecode = True
REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(REPO, "bin", ".local", "lib", "mini-calendar"))
import calevents  # noqa: E402

SAMPLE = """BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//test//EN
BEGIN:VEVENT
UID:single
DTSTART;TZID=Europe/London:20261005T093000
DTEND;TZID=Europe/London:20261005T100000
SUMMARY:Dentist
END:VEVENT
BEGIN:VEVENT
UID:weekly
DTSTART;TZID=Europe/London:20261005T180000
DTEND;TZID=Europe/London:20261005T190000
RRULE:FREQ=WEEKLY;BYDAY=MO
EXDATE;TZID=Europe/London:20261019T180000
SUMMARY:Gym
END:VEVENT
BEGIN:VEVENT
UID:weekly
RECURRENCE-ID;TZID=Europe/London:20261012T180000
DTSTART;TZID=Europe/London:20261013T200000
DTEND;TZID=Europe/London:20261013T210000
SUMMARY:Gym (moved)
END:VEVENT
BEGIN:VEVENT
UID:weekly
RECURRENCE-ID;TZID=Europe/London:20261026T180000
DTSTART;TZID=Europe/London:20261026T180000
DTEND;TZID=Europe/London:20261026T190000
STATUS:CANCELLED
SUMMARY:Gym
END:VEVENT
BEGIN:VEVENT
UID:trip
DTSTART;VALUE=DATE:20261008
DTEND;VALUE=DATE:20261011
SUMMARY:Trip to Wales
END:VEVENT
BEGIN:VEVENT
UID:utc
DTSTART:20261007T120000Z
DTEND:20261007T130000Z
SUMMARY:Call (UTC)
END:VEVENT
BEGIN:VEVENT
UID:cancelled
DTSTART;TZID=Europe/London:20261006T090000
DTEND;TZID=Europe/London:20261006T100000
STATUS:CANCELLED
SUMMARY:Cancelled meeting
END:VEVENT
END:VCALENDAR
"""


class CalendarTests(unittest.TestCase):
    def setUp(self):
        os.environ["TZ"] = "Europe/London"
        import time
        time.tzset()
        self.tmp = tempfile.TemporaryDirectory()
        d = self.tmp.name
        self.ics = os.path.join(d, "sample.ics")
        with open(self.ics, "w") as f:
            f.write(SAMPLE)
        self.config = os.path.join(d, "ics-url")
        with open(self.config, "w") as f:
            f.write(f"# my calendars\n{self.ics}\n")
        self.cache = os.path.join(d, "cache")
        calevents.refresh(force=True, config=self.config, cache=self.cache)

    def tearDown(self):
        self.tmp.cleanup()

    def day(self, y, m, d):
        return [(e.summary, e.all_day, e.start if e.all_day else e.start.strftime("%H:%M"))
                for e in calevents.events_between(dt.date(y, m, d), dt.date(y, m, d), self.config, self.cache)]

    def test_sources_skip_comments_and_get_colors(self):
        src = calevents.sources(self.config)
        self.assertEqual(len(src), 1)
        self.assertEqual(src[0], (calevents.COLORS[0], self.ics))

    def test_single_event_and_weekly_instance(self):
        self.assertEqual(self.day(2026, 10, 5), [("Dentist", False, "09:30"), ("Gym", False, "18:00")])

    def test_moved_instance(self):
        self.assertEqual(self.day(2026, 10, 12), [])
        self.assertEqual(self.day(2026, 10, 13), [("Gym (moved)", False, "20:00")])

    def test_exdate_and_cancelled_instance(self):
        self.assertEqual(self.day(2026, 10, 19), [])
        self.assertEqual(self.day(2026, 10, 26), [])
        self.assertEqual(self.day(2026, 11, 2), [("Gym", False, "18:00")])

    def test_cancelled_event_is_hidden(self):
        self.assertEqual(self.day(2026, 10, 6), [])

    def test_utc_event_in_local_time(self):
        self.assertEqual(self.day(2026, 10, 7), [("Call (UTC)", False, "13:00")])   # BST = UTC+1

    def test_multi_day_all_day_event(self):
        for d in (8, 9, 10):
            self.assertEqual(self.day(2026, 10, d), [("Trip to Wales", True, dt.date(2026, 10, 8))])
        self.assertEqual(self.day(2026, 10, 11), [])

    def test_days_with_events(self):
        days = calevents.days_with_events(2026, 10, self.config, self.cache)
        self.assertEqual(days, {5, 7, 8, 9, 10, 13})

    def test_missing_config_means_no_events(self):
        self.assertEqual(calevents.sources(os.path.join(self.tmp.name, "nope")), [])


if __name__ == "__main__":
    unittest.main()
