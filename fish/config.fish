# ~/.config/fish/config.fish vim: foldmethod=marker

# Define environment variables.
set -x TERMINFO /usr/lib/terminfo
set -x PATH ~/.emacs.d/bin/ ~/.poetry/bin ~/.local/bin/ /opt/zotero/ /opt/nomad/bin/ /opt/mpw/bin /snap/bin $PATH
set -x POETRY_VIRTUALENVS_PATH ~/.virtualenvs
set -x LC_ALL en_US.UTF-8
set -x LC_NUMERIC en_US.UTF-8
set -x FZF_DEFAULT_COMMAND 'fdfind --type f'
set -x FZF_DEFAULT_OPTS $FZF_DEFAULT_OPTS --layout=reverse
set -x NNN_TRASH 1
set -x NNN_USE_EDITOR 1
set -x DOTNET_CLI_TELEMETRY_OPTOUT 1

# Package manager.
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

# Color scheme.
source ~/.config/fisher/github.com/tomyun/base16-fish/functions/base16-solarized-light.fish
source ~/.config/fisher/github.com/nicodebo/base16-fzf/fish/base16-solarized-light.fish
set -x BAT_THEME 'ansi-dark'

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

# Functions and aliases.
function e -d "Edit via $EDITOR" -w nvim
	$EDITOR $argv
end

function i -d "Show image" -w feh
	kitty +kitten icat $argv
end

function man -d "Show full manual" -w man
	if [ -e "$NVIM_LISTEN_ADDRESS" ]
		e +"Man $argv"
	else
		e +"Man $argv" +only
	end
end

function tldr -d "Show tldr manual" -w sudo
	 e +"Tldr $argv"
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
	$EDITOR +Project
end

function z -d 'Open library file'
	for f in (fd -t f -e pdf . ~/snap/zotero-snap/ | fzf -m -d '/' --with-nth=-1 --prompt='Zotero> ')
		zathura $f &
	end
end

function weather -d 'Check the weather forecast'
	curl wttr.in 2>/dev/null | grep -v @
end

function checkip -d 'Check the public IP address'
	curl ifconfig.co
end

function vpn -d 'Connect to VPN'
	expressvpn disconnect
	expressvpn connect \
		( expressvpn list all \
		| tail -n +5 \
		| sed -e 's/[a-z0-9]*\s*\([A-Z].*\)(.*/\1/' -e '/^[a-z]/d' \
		| uniq \
		| fzf --prompt='ExpressVPN> ' )
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

function hours -d 'SINTEF work hours'
	spy ts summary|sed 's/^Download.*/--  --/;s/\s\s\s*/|/g;s/---*/---/g;s/\(.*\)/|\1|/g;s/||/|---|---|/g'
end

function mpz -d 'MasterPassword via FZF'
	read -slP 'MasterPassword> ' pw
	while [ 1 ]
		echo $pw | mpw (/bin/cat ~/.mpw.d/*.mpsites|sed -ne 's/^[^#]\S*\s*\S*\s*\S*\s*\(.*\)/\1/p' | sort | uniq | fzf --prompt 'Website> ' --bind 'alt-enter:print-query') 2>/dev/null | wl-copy
		clear
		echo 'Press Ctrl-C again to quit.'
		sleep 1
	end
end


function fish_title
    set -l command (echo $_)
	if git rev-parse --git-dir > /dev/null ^ /dev/null
		set -l git_dir (git rev-parse --git-dir)
		if [ $git_dir != ".git" ]
			echo git: (basename (dirname $git_dir))
		else
			echo git: (basename (pwd))
		end
	else
		echo dir: (pwd | sed "s|$HOME|~|" )
	end
end
