#------------------------------------------------------------
# File: Shell config
# Path: ~/.config/fish/config.fish
#------------------------------------------------------------

# Enable Vim mode with jk escape.
fish_vi_key_bindings
bind -M insert jk "if commandline -P; commandline -f cancel; else; set fish_bind_mode default; commandline -f backward-char force-repaint; end"

# Define environment variables.
set -x EDITOR "emacsclient -c -a ''"
set -x TERMINFO /usr/lib/terminfo
set -x TERM xterm
set -x PATH ~/.emacs.d/bin/ ~/.poetry/bin ~/.local/bin/ /opt/zotero/ /opt/nomad/bin/ /opt/conda/bin /opt/mpw/bin /snap/bin $PATH
set -x LC_ALL en_US.UTF-8
set -x LC_NUMERIC en_US.UTF-8

# Disable the Fish greeting.
set fish_greeting

# Load the Starship prompt.
if not [ (which starship) ];
   set file (mktemp);
   curl -fsSL https://starship.rs/install.sh > $file;
   mkdir -p ~/.local/bin/;
   bash $file -y -b ~/.local/bin/;
   rm $file;
end
starship init fish | source

# Functions and aliases.
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

# Ensure that I actually use the aliases.
alias stop="echo -e '\e[31;1mUse the alias `e`.\e[0m\n\n'"
alias vim=stop
alias nvim=stop

# Enable vterm directory tracking in Emacs.
function vterm_printf;
    if [ -n "$TMUX" ]
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

function fish_vterm_prompt_end;
    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end

function track_directories --on-event fish_prompt;
    fish_vterm_prompt_end;
end
