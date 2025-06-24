#!/usr/bin/env bash

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
envs_dir="$script_dir/envs"

if [[ $# -eq 0 ]]; then
	dirs=()
	if [[ ! -d $envs_dir ]]; then
		echo "No 'envs' directory found in $script_dir."
		exit 1
	fi
	while IFS= read -r -d $'\0' dir; do
		dirs+=("$(basename "$dir")")
	done < <(find "$envs_dir" -mindepth 1 -maxdepth 1 -type d -print0)
	if [[ ${#dirs[@]} -eq 0 ]]; then
		echo "No environments found in $envs_dir."
		exit 1
	fi
	mapfile -t sorted_dirs < <(printf "%s\n" "${dirs[@]}" | sort)
	echo "Usage: $(basename "$0") <env1> [<env2> ...]"
	echo "Available environments:"
	for d in "${sorted_dirs[@]}"; do
		echo "  $d"
	done
	exit 0
fi

envs=()
for arg in "$@"; do
	if [[ ! -d "$envs_dir/$arg" ]]; then
		echo "Error: '$arg' is not a valid environment directory in $envs_dir."
		exit 1
	fi
	envs+=("$arg")
done

mapfile -t sorted < <(printf "%s\n" "${envs[@]}" | sort)
unset IFS

environment=$(
	IFS=_
	echo "${sorted[*]}"
)

nix develop "$script_dir"#"$environment"
