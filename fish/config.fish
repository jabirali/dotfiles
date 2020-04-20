# ~/.config/fish/config.fish 
# vim: foldmethod=marker

# Environment variables {{{
	# Most of these are set in the `fish_variables` file; however,
	# the ones most commonly changed manually are collected here.
	set -x PATH ~/.local/bin/ ~/.poetry/bin /snap/bin $PATH
	set -g pure_symbol_prompt  "❯"
# }}}

# Bootstrap procedure {{{
	# This section of the config file collects various startup
	# activities, including responding to the environment where
	# fish was started (e.g. within neovim or a virtual env).
	
	# Fish package manager.
	if not functions -q fisher
		curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
		fish -c fisher
	end
	
	# Autoselect best editor.
	if type -q nvr
		if [ -n "$TMUX" ]
			set -x EDITOR nvr
		else
			set -x EDITOR nvim
		end
	else if type -q nvim
		set -x EDITOR nvim
	else if type -q vim
		set -x EDITOR vim
	else
		set -x EDITOR vi
	end
	
	# Virtualenv integration.
	if [ -e "$VIRTUAL_ENV" ]
		source $VIRTUAL_ENV/bin/activate.fish
	end
	
	# Fuzzy-finder integration.
	fzf_key_bindings
# }}}

# Abbreviations {{{
	# The code below automatically replaces classic and portable UNIX tools
	# with modern alternatives that tend to be faster and prettier. However,
	# it does so in a portable way, i.e. only when these are available.
	
	if type -q fdfind
		alias 'fd' 'fdfind'
		abbr -ga 'find' 'fd'
	end
	
	if type -q fd
		abbr -ga 'find' 'fd'
	end
	
	if type -q rg
		abbr -ga 'grep' 'rg'
	end
	
	if type -q exa
		abbr -ga 'ls'   'exa'
		abbr -ga 'll'   'exa -l'
		abbr -ga 'la'   'exa -a'
		abbr -ga 'tree' 'exa -T'
	end
	
	if type -q vim
		abbr -ga 'vi' 'vim'
	end
	
	if type -q nvim
		abbr -ga 'vi'  'nvim'
		abbr -ga 'vim' 'nvim'
	end
	
	if type -q bat
		abbr -ga cat 'bat'
	end
	
	# Provide further abbreviations of common commands.
	abbr -ga 'ga' 'git add'
	abbr -ga 'gc' 'git commit'
	abbr -ga 'gd' 'git diff'
	abbr -ga 'gs' 'git status'
	abbr -ga 'gl' 'git log'
	abbr -ga 'e'  'edit'
	abbr -ga 'o'  'open'
	abbr -ga 'p'  'project'
	abbr -ga 'z'  'zotero'
	
	# Use aliases to provide sensible default arguments.
	alias 'bat' 'bat -p'
	alias 'exa' 'exa --git-ignore --group-directories-first --time-style=long-iso'
# }}}

# Functions {{{
	function fish_right_prompt -d "Update environment variables"
		# Sync the Neovim session to the Tmux workspace, so running `nvr` always reuses 
		# the Neovim of the current workspace. This code is placed in the prompt function 
		# so that it remains up-to-date even after moving Tmux panes between workspaces.
		if [ "$EDITOR" = "nvr" ]
			set -gx NVIM_LISTEN_ADDRESS ~/.cache/nvim/nvr(tmux display -p '#{session_id}#{window_id}')
		end
	end
	
	function edit -d "Edit via $EDITOR" -w nvim
		$EDITOR $argv 2> /dev/null
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
	
	function zotero -d 'Open library file'
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
# }}}
