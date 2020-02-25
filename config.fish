#------------------------------------------------------------
# File: Shell config
# Path: ~/.config/fish/config.fish
#------------------------------------------------------------

# Enable Vim mode with jk escape.
fish_vi_key_bindings
bind -M insert jk "if commandline -P; commandline -f cancel; else; set fish_bind_mode default; commandline -f backward-char force-repaint; end"

# Define environment variables.
set -x EDITOR emacsclient -c -a ''
set -x TERMINFO /usr/lib/terminfo
set -x TERM xterm
set -x PATH ~/.emacs.d/bin/ ~/.poetry/bin ~/.local/bin/ /opt/zotero/ /opt/nomad/bin/ /opt/conda/bin /opt/mpw/bin /snap/bin $PATH
set -x POETRY_VIRTUALENVS_PATH ~/.virtualenvs
set -x LC_ALL en_US.UTF-8
set -x LC_NUMERIC en_US.UTF-8

# Moonlight colorscheme.
set fish_color_autosuggestion "#5b6395"
set fish_color_cancel -r
set fish_color_command "#77e0c6" --bold
set fish_color_comment "#7e8eda" --italics
set fish_color_cwd cyan
set fish_color_end magenta
set fish_color_error red --bold
set fish_color_escape "#89DDFF"
set fish_color_match "#38456A" --bold
set fish_color_operator "#89DDFF" --underline --bold
set fish_color_param blue
set fish_color_quote blue
set fish_color_redirection "#8fd6ff"
set fish_color_search_match --background=brblack
set fish_color_status magenta
set fish_color_user brred
set fish_color_valid_path cyan --underline
set fish_pager_color_completion white
set fish_pager_color_description brblack
set fish_pager_color_prefix yellow

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
    if [ "$INSIDE_EMACS" = "vterm" ]
        printf '\033]51;E%s\033\\' "find-file $argv"
    else
       emacsclient -c -a '' $argv &
    end
end

function vim --description 'Edit in barebones Emacs'
	emacs -q -nw --eval="(setq viper-mode t)" --eval="(viper-mode)" --eval="(load-theme 'wombat)" --eval="(menu-bar-mode -1)" $argv
end

function o --description 'Open in system app'
    xdg-open $argv &
end

function r --description 'Interrupt Emacs'
    killall -SIGUSR2 emacs
end

function venv --description 'Python virtual environments'
    if not count $argv > /dev/null
        echo "Virtual environments:"
        for i in (ls ~/.virtualenvs)
            echo "-" $i
        end
    else if [ -d ~/.virtualenvs/$argv[1] ]
        echo "Activating virtual environment."
        source ~/.virtualenvs/$argv/bin/activate.fish
    else
        echo "The specified virtual environment does not exist."
        echo "Create with `python -m venv ~/.virtualenvs/<name>`."
    end
end

function scrape --description 'Scrape all linked documents from a website'
    wget -r -l 1 -e robots=off
end

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
