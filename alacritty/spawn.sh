#!/bin/sh

# Get the current application PID from Sway.
pid=$(swaymsg -t get_tree | jq '.. | select(.type?) | select(.type=="con") | select(.focused==true) | .pid')

# Get corresponding current working directory.
ppid=$(pgrep --newest --parent $pid)
cwd=$(readlink /proc/$ppid/cwd || echo $HOME)

# Actually run a terminal.
alacritty --working-directory $cwd
