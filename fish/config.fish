# ~/.config/fish/config.fish 
# vim: foldmethod=marker

# Environment variables {{{
	# System settings.
	set -x LANG 'en_US'
	set -x LC_ALL 'en_DK.UTF-8'
	set -x NNN_TRASH 1
	set -x NNN_USE_EDITOR 1
	set -x POETRY_VIRTUALENVS_PATH ~/.virtualenvs
	set -x DOTNET_CLI_TELEMETRY_OPTOUT 1
	
	# Theme settings.
	set -x pure_symbol_prompt  "❯"
	set -x BAT_THEME 'ansi-light'
	set -x FZF_DEFAULT_OPTS '--color=bg+:-1,bg:-1,spinner:#2aa198,hl:#268bd2,fg:#657b83,header:#268bd2,info:#b58900,border:#d7d7af,pointer:#2aa198,marker:#2aa198,fg+:#073642,prompt:#b58900,hl+:#268bd2 --layout=reverse'
	set -x NNN_CONTEXT_COLORS 5555
	
	# ANSI colors.
	set fish_color_autosuggestion normal
	set fish_color_command blue
	set fish_color_comment brred
	set fish_color_end brmagenta
	set fish_color_error brred
	set fish_color_escape bryellow --bold
	set fish_color_normal normal
	set fish_color_operator bryellow
	set fish_color_param black
	set fish_color_quote yellow
	set fish_color_redirection green
	set fish_color_user brgreen
# }}}

# Bootstrap procedure {{{
	# This section of the config file collects essential startup activities.
	
	# Fish package manager.
	if not functions -q fisher
		curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
		fish -c fisher
	end
	
	# Virtualenv integration.
	if [ -e "$VIRTUAL_ENV" ]
		source $VIRTUAL_ENV/bin/activate.fish
	end
	
	# Fuzzy-finder integration.
	fzf_key_bindings
# }}}

# System integration {{{
	# The code below automatically replaces classic and portable UNIX tools
	# with modern alternatives that tend to be faster and prettier. However,
	# it does so in a portable way, i.e. only when these are available.
	
	# Easily open files with its default viewer.
	abbr -ga 'o' 'open'
	function open -d 'Open in system app'
		xdg-open $argv &
	end
	
	# Better replacement for `find`.
	if type -q fd
		set -x FZF_DEFAULT_COMMAND 'fd --type f'
		abbr -ga 'find'   'fd'
		abbr -ga 'fdfind' 'fd'
	else if type -q fdfind
		set -x FZF_DEFAULT_COMMAND 'fdfind --type f'
		alias 'fd' 'fdfind'
		abbr -ga 'find'   'fd'
		abbr -ga 'fdfind' 'fd'
	else
		abbr -ga 'fd'     'find'
		abbr -ga 'fdfind' 'find'
	end
	
	# Better replacement for `grep`.
	if type -q rg
		abbr -ga 'grep' 'rg'
	else
		abbr -ga 'rg'   'grep'
	end
	
	# Better replacement for `ls`.
	if type -q exa
		abbr -ga 'ls'   'exa'
		abbr -ga 'll'   'exa -l'
		abbr -ga 'la'   'exa -a'
		abbr -ga 'tree' 'exa -T'
	else
		abbr -ga 'exa'  'ls'
	end
	
	# Better replacement for `cat`.
	if type -q bat
		abbr -ga 'cat' 'bat'
	else
		abbr -ga 'bat' 'cat'
	end
	
	# Automatic file monitoring.
	if type -q entr
		abbr -ga pytest 'fd \'.py$\' | entr pytest'
	end
	
	# Use aliases to provide sensible default arguments.
	alias 'bat' 'bat -p'
	alias 'exa' 'exa --git-ignore --group-directories-first --time-style=long-iso'
# }}}

# Convenience functions {{{
	# Easy access to Git.
	abbr -ga 'ga' 'git add'
	abbr -ga 'gc' 'git commit'
	abbr -ga 'gd' 'git diff'
	abbr -ga 'gs' 'git status'
	abbr -ga 'gl' 'git log'
	
	abbr -ga 'p' 'project'
	function project -d 'Open project'
		# Discover and select projects.
		set -l dir \
			( fd -HIt d '^\.git$' ~/.config/ ~/Documents/ \
			| sed 's|/\.git$||'               \
			| fzf --prompt 'Project> '        \
			      --query="$argv"             \
			      --delimiter=/ --with-nth=-1 \
			      --preview-window right:65%  \
			      --preview='bat -p --color=always {..}/README{.md,.org,.txt,} 2>/dev/null' )
		
		# Handle the choice made above.
		if [ -n "$dir" ]
			# Switch to the selected project.
			cd "$dir"
			set -l name (basename (pwd))
			set -l venv ~/.virtualenvs/$name
			echo "Switching to \"$name\"."
			
			# Load associated virtualenv.
			if [ -e "$venv" ]
				echo "Activating virtualenv."
				source $venv/bin/activate.fish
			else if type -q deactivate
				echo "Deactivating virtualenv."
				deactivate
			end
		end
		
	end
	
	abbr -ga 'z' 'zotero'
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
	
	abbr -ga 'v' 'venv'
	function venv -d 'Python virtual environments'
		# Determine the venv name.
		set -l name (basename (pwd))
		set -l venv ~/.virtualenvs/$name
		
		# Create the venv if neccessary.
		if [ ! -d "$venv" ]
			echo "Creating virtual environment."
			python3 -m venv "$venv"
		end
		
		# Activate the venv in any case.
		echo "Activating virtual environment."
		source "$venv/bin/activate.fish"
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
