# ~/.config/fish/config.fish vim: foldmethod=marker

# Package manager.
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

# Fancy prompt.
if not [ (which starship) ];
   set file (mktemp);
   curl -fsSL https://starship.rs/install.sh > $file;
   mkdir -p ~/.local/bin/;
   bash $file -y -b ~/.local/bin/;
   rm $file;
end
starship init fish | source
set fish_greeting ""

# Neovim integration.
if [ -e "$NVIM_LISTEN_ADDRESS" ]
	set -x EDITOR nvr
else
	set -x EDITOR nvim
end

# Virtualenv integration.
if [ -e "$VIRTUAL_ENV" ]
	source $VIRTUAL_ENV/bin/activate.fish
end

# Fuzzy-finder integration.
fzf_key_bindings

# Use abbreviations to switch from UNIX classics.
abbr -ga cat  'bat'
abbr -ga find 'fd'
abbr -ga grep 'rg'
abbr -ga ll   'exa -l'
abbr -ga ls   'exa'
abbr -ga tree 'exa -T'
abbr -ga vim  'nvim'
abbr -ga vi   'nvim'

# Use aliases to provide sensible default arguments.
alias bat 'bat -p'
alias exa 'exa --git --git-ignore --group-directories-first --time-style=long-iso'
alias fd  'fdfind'

# Define environment variables.
set -x TERMINFO /usr/lib/terminfo
set -x PATH ~/.emacs.d/bin/ ~/.poetry/bin ~/.local/bin/ /opt/zotero/ /opt/nomad/bin/ /opt/mpw/bin /snap/bin $PATH
set -x POETRY_VIRTUALENVS_PATH ~/.virtualenvs
set -x LC_ALL en_US.UTF-8
set -x LC_NUMERIC en_US.UTF-8
set -x FZF_DEFAULT_COMMAND 'fdfind --type f'
set -x FZF_DEFAULT_OPTS '--color fg:-1,bg:-1,fg+:-1,bg+:#2a2e48,hl:#f79a62,hl+:#ffc777,pointer:#ffc777,marker:#c3a2ff,info:#c3a2ff,prompt:#7e8eda,border:#7e8eda,spinner:#7e8eda,header:#7e8eda --layout=reverse'
set -x NNN_TRASH = 1
set -x NNN_USE_EDITOR = 1

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

# Functions and aliases.
function e -d "Edit via $EDITOR" -w nvim
	$EDITOR $argv
end

function i -d "Show image" -w feh
	kitty +kitten icat $argv
end

function man -d "Show full manual" -w man
	 $EDITOR +"Man $argv" +only
end

function tldr -d "Show tldr manual" -w sudo
	 $EDITOR +"Tldr $argv" +only
end

function d -d 'File manager'
    # Block nesting in subshells.
    if test -n NNNLVL
        if [ (expr $NNNLVL + 0) -ge 1 ]
            echo "nnn is already running"
            return
        end
    end

    # Output file for `cd` info.
    if [ -n "$XDG_CONFIG_HOME" ]
        set -x NNN_TMPFILE "$XDG_CONFIG_HOME/nnn/.lastd"
    else
        set -x NNN_TMPFILE "$HOME/.config/nnn/.lastd"
	end

	# Launch `nnn`.
	nnn $argv

	# Implement `cd` on quit.
    if [ -e $NNN_TMPFILE ]
        source $NNN_TMPFILE
        rm $NNN_TMPFILE
    end
end

function o -d 'Open in system app'
    xdg-open $argv &
end

function p -d 'Open project'
	cd (ls -d ~/projects/* | fzf -d / --with-nth=-1 --prompt='Project: ' \
		--preview="bat --style=plain --color=always {..}/README{.md,.org,.txt,} 2>/dev/null")
end

function z -d 'Open library file'
	for f in (fd -t f -e pdf . ~/.zotero/ | fzf -m -d '/' --with-nth=-1 --prompt='Zotero: ')
		zathura $f &
	end
end

function venv -d 'Python virtual environments'
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

function wget! -d 'Scrape all linked documents from a website'
    wget -r -l 1 -e robots=off
end

function nup -d 'Update Neovim plugins'
    nvim +PlugStatus +only +PlugInstall +PlugUpdate +PlugUpgrade +UpdateRemotePlugins +qa
end

function fish_title
    set -l command (echo $_)
    if test $command = "fish"
        if git rev-parse --git-dir > /dev/null ^ /dev/null
            set -l git_dir (git rev-parse --git-dir)
            if test $git_dir = ".git"
                echo (basename (pwd))
            else
                echo (basename (dirname $git_dir))
            end
        else
            echo (pwd)
        end
    else
        echo $command
    end
end
