#!/usr/bin/env bash
# Printers and scanner on the home network.
#
#   Brother MFC-L2700DN  network printer and scanner at 192.168.0.11
#                        printing: driverless IPP Everywhere, two-sided default
#                        scanning: sane-airscan over WSD (no Brother driver)
#   Canon iX6800         USB printer shared by the CUPS server hs-media-srv
#                        (192.168.0.137), which runs the Canon driver
#
# cups-browsed is disabled so the server's shared queues do not show up twice.
# The scanner is pinned by address in sane-airscan because the auto-discovered
# entry only exists once WSD discovery finishes and failed to open at times;
# that entry is blacklisted. Scan with simple-scan ("Document Scanner").
set -euo pipefail

BROTHER=192.168.0.11
SERVER=192.168.0.137

sudo dnf install -y cups simple-scan sane-backends sane-airscan
sudo systemctl enable --now cups
sudo systemctl disable --now cups-browsed 2>/dev/null || true

sudo lpadmin -p Brother_MFC_L2700DN -D "Brother MFC-L2700DN" -L "Network $BROTHER" \
  -v "ipp://$BROTHER/ipp/print" -m everywhere -E
sudo lpadmin -p Brother_MFC_L2700DN -o Duplex=DuplexNoTumble
sudo lpadmin -p Canon_iX6800 -D "Canon iX6800" -L "USB on hs-media-srv ($SERVER)" \
  -v "ipp://$SERVER:631/printers/IX6800USB" -m everywhere -E
sudo lpadmin -p Canon_iX6800 -o print-color-mode-default=color
sudo lpadmin -d Brother_MFC_L2700DN

conf=/etc/sane.d/airscan.conf
grep -q 'MFC-L2700DN" =' $conf ||
  sudo sed -i "/^\[devices\]/a \"Brother MFC-L2700DN\" = http://$BROTHER:80/WebServices/ScannerService, WSD" $conf
grep -q '^name = "Brother MFC-L2700DN series"' $conf ||
  sudo sed -i '/^\[blacklist\]/a name = "Brother MFC-L2700DN series"' $conf

lpstat -p -d
scanimage -L | grep -v v4l
