#!/usr/bin/env bash

TARGET_WS=$1

# Check if the target workspace is currently displayed on any output
if swaymsg -t get_workspaces | jq ".[] | select(.num == $TARGET_WS and .visible == true)" | grep -q .; then
	# If the workspace is focused, switch to it
	swaymsg "workspace $TARGET_WS"
	exit 0
fi

# Get the current output
current_output=$(swaymsg -t get_outputs | jq -r '.[] | select(.focused == true) | .name' | head -n 1)

# Switch to the workspace and move it to the current output
swaymsg "workspace $TARGET_WS; move workspace to output $current_output"
