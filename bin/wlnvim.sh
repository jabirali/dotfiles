#!/bin/bash

# This is a small script to edit any text selection
# in Wayland via a pop-up terminal with NeoVim in it.

# Create temporary file
tmp=$(mktemp)

# Get primary selection from Wayland clipboard
wl-paste -p > $tmp

# Open Kitty + NeoVim to edit the selection
kitty --title 'Selection editor' -e nvim $tmp

# Flush the result back to the clipboard
wl-copy < $tmp

# Remove temporary file
rm $tmp
