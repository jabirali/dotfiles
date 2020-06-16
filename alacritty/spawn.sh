#!/bin/sh

# Get the current application PID from Sway.
pid=$(swaymsg -t get_tree | jq '.. | select(.type?) | select(.type=="con") | select(.app_id=="Alacritty") | select(.focused==true) | .pid')

# Get corresponding current working directory.
cwd=$(readlink /proc/$(pgrep --newest --parent "$pid")/cwd || echo $HOME)

# Actually run a terminal.
alacritty --working-directory $cwd
