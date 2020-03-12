#!/bin/bash

# This is a small script to edit any text selection in Wayland via a 
# pop-up terminal with Neovim in it.  This version of the script edits
# the file as a Markdown format, and converts to HTML mime after.

# Create temporary file.
tmp=$(mktemp)

# Get primary selection from Wayland clipboard.
wl-paste -t text/html -p > $tmp

# Convert to Markdown.
format='markdown-raw_html-native_divs-native_spans-fenced_divs-bracketed_spans-header_attributes'
pandoc -f html --atx-headers --reference-links -t $format $tmp | sponge $tmp

# Open Kitty + Neovim to edit the selection.
kitty --title 'Markdown editor' -e nvim +'set ft=markdown' $tmp

# Convert back to HTML.
pandoc -f markdown -t html -s $tmp | sponge $tmp

# Prepend a meta header.
meta='<meta http-equiv="content-type" content="text/html; charset=utf-8">'
echo $meta | cat - $tmp | sponge $tmp

# Flush the result back to the clipboard
cat $tmp | wl-copy -t text/html
cat $tmp | wl-copy -t text/html -p

# Remove temporary file
rm $tmp
