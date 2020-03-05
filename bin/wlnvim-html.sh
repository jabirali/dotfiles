#!/bin/bash

# This is a small script to edit any text selection
# in Wayland via a pop-up terminal with NeoVim in it.
# This version of the script edits the file as a
# Markdown format, and converts to HTML mime after.

# Create temporary file
tmp=$(mktemp)

# Get primary selection from Wayland clipboard
wl-paste -t text/html -p | pandoc -f html -t markdown > $tmp

# Open Kitty + NeoVim to edit the selection
kitty --title 'Markdown editor' -e nvim +'set ft=markdown' $tmp

# Flush the result back to the clipboard
pandoc -f markdown -t html $tmp | wl-copy -t text/html
pandoc -f markdown -t html $tmp | wl-copy -t text/html -p

# Remove temporary file
rm $tmp
