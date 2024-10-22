#!/usr/bin/env bash

my_volume() {
	volumep=$(wpctl get-volume @DEFAULT_AUDIO_SINK@)

	case "$volumep" in
	*MUTED*) mute='M' ;;
	esac

	case "$volumep" in
	*0.0*) volumep="${volumep##*0.0}" ;;
	*0.*) volumep="${volumep##*0.}" ;;
	*1.00*) volumep=100 ;;
	*)
		wpctl set-volume @DEFAULT_AUDIO_SINK@ 1
		volumep=100
		;;
	esac

	echo "${volumep%% *}${mute}"
}

my_date_time() {
	date +'%Y%m%dT%H%M%S'
}

my_battery_capacity() {
	if [ -d /sys/class/power_supply/BAT0 ]; then
		cat /sys/class/power_supply/BAT0/capacity
	fi
}

while true; do
	echo "$(my_battery_capacity)_$(my_volume)_$(my_date_time)"
	sleep 0.1
done
