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

# Environment variables
set EDITOR e
set TERMINFO /usr/lib/terminfo
set TERM xterm-256color
set PATH /opt/zotero/ /opt/nomad/bin/ /opt/conda/bin /opt/mpw/bin /snap/bin $PATH
set LC_ALL en_US.UTF-8
set LC_NUMERIC en_US.UTF-8

# Functions and aliases
function e --description 'Edit in Emacs'
    emacsclient -c -a '' $argv &
end

function o --description 'Open in system app'
    xdg-open $argv &
end

function r --description 'Interrupt Emacs'
    killall -SIGUSR2 emacs
end

function venv --description 'Create or activate a virtual environment'
    python -m venv .venv
    source .venv/bin/activate.fish
    pip install --upgrade pip setuptools > /dev/null
end

function scrape --description 'Scrape all linked documents from a website'
    wget -r -l 1 -e robots=off
end

# Ensure that I actually use the aliases
alias stop="echo -e '\e[31;1mUse the alias `e`.\e[0m\n\n'"
alias vim=stop
alias nvim=stop
