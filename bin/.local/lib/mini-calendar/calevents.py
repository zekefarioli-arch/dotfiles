"""Events from Google Calendar (or any iCal feed) for the mini calendar.

Sources are read from ~/.config/mini-calendar/ics-url, one per line (the
"secret address in iCal format" of each Google calendar, or a local path).
That file is private: it is not part of the dotfiles. Feeds are cached in
~/.cache/mini-calendar and refreshed when older than 15 minutes.
"""
import datetime as dt
import os
import urllib.request
from dataclasses import dataclass

from dateutil.rrule import rrulestr
from icalendar import Calendar

CONFIG = os.path.expanduser("~/.config/mini-calendar/ics-url")
CACHE = os.path.expanduser("~/.cache/mini-calendar")
MAX_AGE = 15 * 60
COLORS = ["#f5c2e7", "#89b4fa", "#a6e3a1", "#fab387", "#cba6f7", "#94e2d5"]  # Catppuccin


@dataclass
class Event:
    start: object        # date (all-day) or aware datetime in local time
    end: object
    summary: str
    color: str
    all_day: bool


def sources(config=CONFIG):
    """[(color, url-or-path)] from the config file, ignoring comments."""
    if not os.path.exists(config):
        return []
    with open(config, encoding="utf-8") as f:
        urls = [l.strip() for l in f if l.strip() and not l.lstrip().startswith("#")]
    return [(COLORS[i % len(COLORS)], u) for i, u in enumerate(urls)]


def cache_file(index, cache=CACHE):
    return os.path.join(cache, f"calendar-{index}.ics")


def refresh(force=False, config=CONFIG, cache=CACHE):
    """Download the feeds whose cache is missing or older than MAX_AGE.
    Returns True if anything changed. Network errors keep the old cache."""
    os.makedirs(cache, exist_ok=True)
    changed = False
    for i, (_, src) in enumerate(sources(config)):
        path = cache_file(i, cache)
        if not force and os.path.exists(path) and dt.datetime.now().timestamp() - os.path.getmtime(path) < MAX_AGE:
            continue
        try:
            if src.startswith(("http://", "https://")):
                with urllib.request.urlopen(src, timeout=10) as r:
                    data = r.read()
            else:
                with open(os.path.expanduser(src.removeprefix("file://")), "rb") as f:
                    data = f.read()
        except OSError:
            continue
        with open(path + ".tmp", "wb") as f:
            f.write(data)
        os.replace(path + ".tmp", path)
        changed = True
    return changed


def _local(value):
    """Dates stay dates; datetimes become aware local datetimes."""
    if isinstance(value, dt.datetime):
        if value.tzinfo is None:
            value = value.astimezone()          # floating time: treat as local
        return value.astimezone()
    return value


def _instances(event, range_start, range_end):
    """Start times of an event (expanding RRULE, minus EXDATE) that may
    overlap [range_start, range_end)."""
    start = event.decoded("DTSTART")
    if "RRULE" not in event:
        return [start]
    all_day = not isinstance(start, dt.datetime)
    base = dt.datetime.combine(start, dt.time()) if all_day else start
    rule = rrulestr(event["RRULE"].to_ical().decode(), dtstart=base)
    lo, hi = range_start - dt.timedelta(days=31), range_end   # catch long events
    if base.tzinfo is None:
        lo, hi = lo.replace(tzinfo=None), hi.replace(tzinfo=None)
    else:
        lo, hi = lo.astimezone(base.tzinfo), hi.astimezone(base.tzinfo)
    excluded = set()
    for exdate in (event.get("EXDATE") if isinstance(event.get("EXDATE"), list) else [event.get("EXDATE")]):
        if exdate is not None:
            for d in exdate.dts:
                v = d.dt
                excluded.add(v.date() if all_day and isinstance(v, dt.datetime) else v)
    out = []
    for occ in rule.between(lo, hi, inc=True):
        value = occ.date() if all_day else occ
        if value not in excluded:
            out.append(value)
    return out


def events_between(first_day, last_day, config=CONFIG, cache=CACHE):
    """Events overlapping the days first_day..last_day (inclusive), sorted
    with all-day events first."""
    tz = dt.datetime.now().astimezone().tzinfo
    range_start = dt.datetime.combine(first_day, dt.time(), tz)
    range_end = dt.datetime.combine(last_day + dt.timedelta(days=1), dt.time(), tz)
    result = []
    for i, (color, _) in enumerate(sources(config)):
        path = cache_file(i, cache)
        if not os.path.exists(path):
            continue
        with open(path, "rb") as f:
            cal = Calendar.from_ical(f.read())
        events = [c for c in cal.walk("VEVENT")]
        # Modified or cancelled instances of recurring events (RECURRENCE-ID)
        overrides = {}
        for e in events:
            if "RECURRENCE-ID" in e:
                overrides[(str(e.get("UID")), _local(e.decoded("RECURRENCE-ID")))] = e
        for e in events:
            if "RECURRENCE-ID" in e or str(e.get("STATUS", "")).upper() == "CANCELLED":
                continue
            start0 = e.decoded("DTSTART")
            if "DTEND" in e:
                length = e.decoded("DTEND") - start0
            elif "DURATION" in e:
                length = e.decoded("DURATION")
            else:
                length = dt.timedelta(days=1) if not isinstance(start0, dt.datetime) else dt.timedelta()
            for start in _instances(e, range_start, range_end):
                item, s, length_i = e, start, length
                key = (str(e.get("UID")), _local(start))
                if key in overrides:
                    item = overrides[key]
                    if str(item.get("STATUS", "")).upper() == "CANCELLED":
                        continue
                    s = item.decoded("DTSTART")
                    length_i = (item.decoded("DTEND") - s) if "DTEND" in item else length
                all_day = not isinstance(s, dt.datetime)
                s, end = _local(s), _local(s + length_i)
                lo = dt.datetime.combine(s, dt.time(), tz) if all_day else s
                hi = dt.datetime.combine(end, dt.time(), tz) if all_day else end
                if hi > range_start and lo < range_end or (lo == hi and range_start <= lo < range_end):
                    result.append(Event(s, end, str(item.get("SUMMARY", "(no title)")), color, all_day))
    result.sort(key=lambda ev: (not ev.all_day, dt.datetime.combine(ev.start, dt.time(), tz) if ev.all_day else ev.start, ev.summary))
    return result


def days_with_events(year, month, config=CONFIG, cache=CACHE):
    """Set of day numbers in the month that have at least one event."""
    first = dt.date(year, month, 1)
    last = (first + dt.timedelta(days=32)).replace(day=1) - dt.timedelta(days=1)
    days = set()
    for ev in events_between(first, last, config, cache):
        s = ev.start if ev.all_day else ev.start.date()
        e = ev.end if ev.all_day else ev.end.date()
        d = max(s, first)
        while d <= last and (d < e or d == s):
            days.add(d.day)
            d += dt.timedelta(days=1)
    return days
