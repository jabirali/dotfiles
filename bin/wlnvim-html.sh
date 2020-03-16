#!/bin/bash

# This is a small script to edit any text selection in Wayland via a 
# pop-up terminal with Neovim in it.  This version of the script edits
# the file as a Markdown format, and converts to HTML mime after.

# Generate filename.
file="/tmp/vim-$(date +%F_%H:%M:%S)"

# Get primary selection from Wayland clipboard.
wl-paste -t text/html -p > $file.html

# Convert to Markdown.
format='markdown-raw_html-native_divs-native_spans-fenced_divs-bracketed_spans-header_attributes'
pandoc -f html --atx-headers --reference-links -t $format $file.html > $file.md

# Open Kitty + Neovim to edit the selection.
kitty --title 'vim: clip' -e nvim $file.md

# Convert back to HTML.
pandoc -f markdown -t html -s --highlight-style=tango $file.md > $file.html

# Prepend a meta header.
meta='<meta http-equiv="content-type" content="text/html; charset=utf-8">'
echo $meta | cat - $file.html | sponge $file.html

# Flush the result back to the clipboard
cat $file.html | wl-copy -t text/html
cat $file.html | wl-copy -t text/html -p

# Make the files only readable for me.
chmod 600 $file.{md,html}
