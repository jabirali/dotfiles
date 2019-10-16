#------------------------------------------------------------
# File: Shell config
# Path: ~/.config/fish/config.fish
#------------------------------------------------------------

# Package manager
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end 

# Set theme
theme_gruvbox dark medium
set -g theme_color_scheme gruvbox
set -g theme_powerline_fonts no
set -g theme_newline_cursor yes
set -g theme_newline_prompt "▶ "
set -g theme_display_virtualenv no
set -g theme_display_vi no
set -g theme_date_format "+%H:%M"

# Environment variables
set EDITOR nvim
set PATH /opt/zotero/ /opt/nomad/bin/ /opt/conda/bin $PATH
set LC_ALL en_US.UTF-8
set LC_NUMERIC en_US.UTF-8

# Keybindings
fish_vi_key_bindings

# Local aliases
alias onedrive="rclone --vfs-cache-mode writes mount sintef: /data/onedrive"
