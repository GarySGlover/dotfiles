#!/usr/bin/env bash
systemctl --user stop hyprctl-submap.service
systemd-run --user --on-active=2 --timer-property=AccuracySec=1us --unit=hyprctl-submap.service -- hyprctl dispatch submap insert
