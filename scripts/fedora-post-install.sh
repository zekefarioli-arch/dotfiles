#!/usr/bin/env bash
# Fedora post-install setup for this laptop (ThinkPad X13 Gen 1 AMD, btrfs).
# Idempotent: safe to run again. Firmware updates are only listed, not applied
# (they need AC power and a reboot): sudo fwupdmgr update
set -euo pipefail
DOTFILES="$(cd "$(dirname "$0")/.." && pwd)"

echo "==> dnf: parallel downloads"
grep -q '^max_parallel_downloads' /etc/dnf/dnf.conf ||
  echo 'max_parallel_downloads=10' | sudo tee -a /etc/dnf/dnf.conf >/dev/null

echo "==> RPM Fusion (free + nonfree) and Cisco OpenH264"
sudo dnf install -y \
  "https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm" \
  "https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm"
sudo dnf config-manager setopt fedora-cisco-openh264.enabled=1

echo "==> Multimedia codecs (full ffmpeg + GStreamer)"
if rpm -q ffmpeg-free >/dev/null 2>&1; then
  sudo dnf swap -y ffmpeg-free ffmpeg --allowerasing
else
  sudo dnf install -y ffmpeg
fi
sudo dnf install -y --setopt=install_weak_deps=False \
  gstreamer1-plugins-good gstreamer1-plugins-bad-free gstreamer1-plugins-bad-freeworld \
  gstreamer1-plugins-ugly gstreamer1-plugins-ugly-free gstreamer1-plugin-openh264 \
  gstreamer1-plugin-libav --exclude=PackageKit-gstreamer-plugin

echo "==> AMD hardware video decoding/encoding (VA-API freeworld)"
if rpm -q mesa-va-drivers >/dev/null 2>&1; then
  sudo dnf swap -y mesa-va-drivers mesa-va-drivers-freeworld
else
  sudo dnf install -y mesa-va-drivers-freeworld
fi
sudo dnf install -y libva-utils

echo "==> Snapper: snapshots of / (not /home) around every dnf transaction"
sudo dnf install -y snapper libdnf5-plugin-actions
sudo snapper -c root list >/dev/null 2>&1 || sudo snapper -c root create-config /
sudo snapper -c root set-config TIMELINE_CREATE=no NUMBER_LIMIT=10 NUMBER_LIMIT_IMPORTANT=5 \
  ALLOW_USERS="$USER" SYNC_ACL=yes
sudo systemctl enable --now snapper-cleanup.timer
sudo install -D -m 644 "$DOTFILES/system/etc/dnf/libdnf5-plugins/actions.d/snapper.actions" \
  /etc/dnf/libdnf5-plugins/actions.d/snapper.actions

echo "==> Power profiles (balanced / power-saver / performance)"
sudo dnf install -y power-profiles-daemon
sudo systemctl enable --now power-profiles-daemon

echo "==> Firmware updates available (apply with: sudo fwupdmgr update)"
sudo fwupdmgr refresh --force >/dev/null 2>&1 || true
sudo fwupdmgr get-updates || true
