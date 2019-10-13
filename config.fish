#------------------------------------------------------------
# File: Shell config
# Path: ~/.config/fish/config.fish
#------------------------------------------------------------

# Set theme
theme_gruvbox dark medium

# Set prompt
set -g theme_display_virtualenv no
set -g theme_color_scheme gruvbox
set -g theme_display_git no
set -g theme_date_format "+%H:%M"

# Environment variables
set EDITOR nvim
set PATH /opt/zotero/ /opt/nomad/bin/ /opt/conda/bin $PATH
set LC_ALL en_US.UTF-8
set LC_NUMERIC en_US.UTF-8

# Local aliases
alias onedrive="rclone --vfs-cache-mode writes mount sintef: /data/onedrive"
