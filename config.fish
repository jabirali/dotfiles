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

# Color theme
#theme_gruvbox dark medium

# Environment variables
set EDITOR e
set PATH /opt/zotero/ /opt/nomad/bin/ /opt/conda/bin /opt/mpw/bin $PATH
set LC_ALL en_US.UTF-8
set LC_NUMERIC en_US.UTF-8

# Keybindings
#fish_vi_key_bindings

# Global aliases
alias o="xdg-open"
alias e="/opt/emacs/bin/emacsclient -nw -c -a ''"
alias t="/opt/emacs/bin/emacsclient -c -a '' -e '(eshell t)'"
alias v="tilix --action=app-new-session -e"
alias r="killall -SIGUSR2 emacs"

alias venv="python -m venv .venv && source .venv/bin/activate.fish && pip install --upgrade pip setuptools > /dev/null"

# Use the aliases...
alias stop="clear; echo -e '\e[31;1mNO! Use the alias `e`.\e[0m\n\n'; sleep 3; e"
alias vim=stop
alias nvim=stop
