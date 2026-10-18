#!/bin/sh
# CI virtual machine only (the Ubuntu Server guest `ci-runner`, not this desktop). Run as root by
# ci-trim-memory.timer every minute. Gives the memory the VM is not using back to the host.
#
# Why: the VM keeps what its jobs read (node_modules, images, browsers) as disk cache, and the
# free memory it has left is scattered in small pieces. Both look "used" to the host, and the VM's
# automatic free page reporting only gives back free blocks of 2 MiB or more, so after a job the
# host kept about 5 GiB of a VM that used 1.4 GiB. Dropping the cache and compacting the memory
# makes big free blocks again, and the reporting then returns them (within seconds).
#
# When it acts: no job is running (no `Runner.Worker` process) AND the 1-minute load average is
# below 0.30 AND there are 300 MiB of cache or more to give back. Otherwise it does nothing, and
# says nothing. Why the load as well as the missing job: between two jobs of a queue there is a
# gap of a few seconds with no `Runner.Worker`, and emptying the cache then would slow the next
# job for nothing; the load of the minute that just ended is still high in that gap, and it falls
# under 0.30 about 1.5 to 2 minutes after the last job (the 5-minute average took about 9). The
# cost of acting is that the next job reads its files from disk once more. It never touches a
# running job: dropping the cache only makes things slower, it cannot lose data (`sync` first).
#
#   ci-trim-memory.sh          what the timer runs
#   ci-trim-memory.sh --now    ignore the load and the size of the cache (never a running job)
#
# It runs every minute and is silent unless it trims: see what it did with the journal (below).
#
# Install (inside the VM, as root), from the folder that holds these three files:
#   install -m 755 ci-trim-memory.sh /usr/local/sbin/ci-trim-memory.sh
#   install -m 644 ci-trim-memory.service ci-trim-memory.timer /etc/systemd/system/
#   systemctl daemon-reload && systemctl enable --now ci-trim-memory.timer
# See what it did:  journalctl -t ci-trim-memory
# Undo: systemctl disable --now ci-trim-memory.timer; rm /usr/local/sbin/ci-trim-memory.sh \
#       /etc/systemd/system/ci-trim-memory.service /etc/systemd/system/ci-trim-memory.timer
#
# `ci-status --now` runs it over ssh as the VM user `ci` with `sudo -n`: that user needs sudo for this
# script (today it has NOPASSWD:ALL from cloud-init; a narrower rule would be `ci ALL=(root) NOPASSWD:
# /usr/local/sbin/ci-trim-memory.sh`, without SETENV).
# The CI_TRIM_* variables only exist so the tests can run it without root (tests/test_ci_trim_memory.py).
set -eu
# The CI_TRIM_* variables are for the tests. As root they are ignored, so nobody who can run this
# script through sudo with a changed environment can make it write into other files.
if [ "$(id -u)" = 0 ]; then
    unset CI_TRIM_PROC_VM CI_TRIM_MEMINFO CI_TRIM_LOADAVG CI_TRIM_MIN_CACHE_MIB CI_TRIM_MAX_LOAD1 CI_TRIM_SETTLE_SECONDS
fi
proc_vm=${CI_TRIM_PROC_VM:-/proc/sys/vm}
meminfo=${CI_TRIM_MEMINFO:-/proc/meminfo}
loadavg=${CI_TRIM_LOADAVG:-/proc/loadavg}
min_cache_mib=${CI_TRIM_MIN_CACHE_MIB:-300}
max_load1=${CI_TRIM_MAX_LOAD1:-0.30}
settle=${CI_TRIM_SETTLE_SECONDS:-4}

log() { logger -t ci-trim-memory -- "$*"; }
mib() { awk -v k="$1:" '$1 == k {printf "%d", $2 / 1024}' "$meminfo"; }

# A job is running: leave everything alone. (Silent: this runs every minute.)
# Only pgrep's exit code 1 means "no such process". Any other code (2 and 3: it could not look,
# 127: it is not installed) is "I do not know", and not knowing is not a reason to drop the cache.
pg=0
pgrep -x Runner.Worker >/dev/null 2>&1 || pg=$?
[ "$pg" -eq 1 ] || exit 0

force=0
[ "${1:-}" = "--now" ] && force=1
cached=$(mib Cached)
free=$(mib MemFree)

if [ "$force" = 0 ]; then
    load1=$(awk '{print $1}' "$loadavg")
    if awk -v l="$load1" -v m="$max_load1" 'BEGIN {exit !(l >= m)}'; then
        exit 0
    fi
    # Nothing to give back (a trim a few minutes ago, or no job for a while): stay quiet.
    [ "$cached" -ge "$min_cache_mib" ] || exit 0
fi

sync
echo 3 > "$proc_vm/drop_caches"
echo 1 > "$proc_vm/compact_memory"
sleep "$settle"   # the host is told a couple of seconds after the blocks become free
log "trimmed: cache ${cached} -> $(mib Cached) MiB, free ${free} -> $(mib MemFree) MiB"
