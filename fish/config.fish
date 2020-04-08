# ~/.config/fish/config.fish vim: foldmethod=marker

# Define environment variables.
set -x PATH ~/.poetry/bin ~/.local/bin/ /opt/zotero/ /opt/mpw/bin /snap/bin $PATH
set -x FISH_TTY (tty)

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
alias exa 'exa --git-ignore --group-directories-first --time-style=long-iso'
alias fd  'fdfind'

# Functions and aliases.
function e -d "Edit via $EDITOR" -w nvim
	$EDITOR $argv
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

function open -d 'Open in system app'
    xdg-open $argv &
end

function project -d 'Open project'
	cd ( fdfind -HIt d '^\.git$' ~/projects/ | sed 's|/\.git$||' \
	   | fzf --prompt 'Project> ' -d / --with-nth=-1 --preview-window right:65% \
	         --preview='bat -p --color=always {..}/README{.md,.org,.txt,} 2>/dev/null' )
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

function ssh-fzf -d 'Connect via SSH'
	while true
		ssh ( sed -ne 's/^\s*Host \([^*].*\)/\1/p' ~/.ssh/config \
		    | fzf --prompt "SSH> " --preview="echo -e '\e[31m# Workspaces\e[0m'; ssh {} tmux list-windows 2>/dev/null || echo 'None.'") \
		          -tt "fish -c \"tmux attach || tmux\" || tmux attach || tmux || fish || bash"
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

function man -d "Show long manual" -w man
	if [ -e "$NVIM_LISTEN_ADDRESS" ]
		e +"Man $argv"
	else
		e +"Man $argv" +only
	end
end

function tldr -d "Show short manual" -w sudo
	if [ -e "$NVIM_LISTEN_ADDRESS" ]
		e +"Tldr $argv"
	else
		e +"Tldr $argv" +only
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
