# ~/.config/fish/config.fish 
# vim: foldmethod=marker

# The most interesting parts of my `config.fish` has been exported as separate 
# Fish plugins on github.com/jabirali/fish-*, which can be installed via `fisher`.
# The specific list of Fish plugins that I use are registered in my `fishfile`.

# Fish package manager.
if not functions -q fisher
	curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
	fish -c fisher
end

# Environment variables {{{
	# System settings.
	set -x LANG 'en_US'
	set -x LC_ALL 'en_DK.UTF-8'
	set -x NNN_TRASH 1
	set -x NNN_USE_EDITOR 1
	set -x POETRY_VIRTUALENVS_PATH ~/.virtualenvs
	set -x DOTNET_CLI_TELEMETRY_OPTOUT 1
	
	# Theme settings.
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
	
	# Plugin settings.
	set -x projector_dir ~/Documents
	set -x projector_pip poetry black
	set -x zotfile_root ~/snap/zotero-snap
# }}}

# Aliases and abbreviations {{{
	# Easily open files with its default viewer.
	abbr -ga 'o' 'open'
	function open -d 'Open in system app'
		xdg-open $argv &
	end
	
	# Automatic file monitoring.
	if type -q entr
		abbr -ga pytest 'fd \'.py$\' | entr pytest'
	end
	
	# Use aliases to provide sensible default arguments.
	alias 'exa' 'exa --git-ignore --group-directories-first --time-style=long-iso'
	
	# Easy access to Git.
	abbr -ga 'ga' 'git add'
	abbr -ga 'gc' 'git commit'
	abbr -ga 'gd' 'git diff'
	abbr -ga 'gs' 'git status'
	abbr -ga 'gl' 'git log'
	
	# Easy access to APT.
	abbr -ga 'as' 'apt search'
	abbr -ga 'ai' 'sudo apt install'
	abbr -ga 'ar' 'sudo apt autoremove'
	abbr -ga 'au' 'sudo apt upgrade'
	
	# Easy access to Snap.
	abbr -ga 'ss' 'snap search'
	abbr -ga 'si' 'sudo snap install'
	abbr -ga 'sr' 'sudo snap remove'
	abbr -ga 'su' 'sudo snap refresh'
	
	# Misc abbreviations.
	abbr -ga 'z' 'zotfile'
	
	# Fuzzy-finder integration.
	fzf_key_bindings
# }}}

# Convenience functions {{{
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
	
	function wget! -d 'Scrape all linked documents from a website'
		wget -r -l 1 -e robots=off
	end
	
	abbr -ga u update
	function update -d 'Update all command-line tools'
		echo -e "\n\e[1m:: Neovim plugins\e[0m"
		nvim +PlugStatus +only +PlugInstall +PlugUpdate +PlugUpgrade +UpdateRemotePlugins +qa
		
		echo -e "\n\e[1m:: Fish plugins\e[0m"
		fisher
		
		echo -e "\n\e[1m:: Tmux plugins\e[0m"
		echo -e 'Installing plugins!\n'
		~/.tmux/plugins/tpm/bin/install_plugins
		echo
		~/.tmux/plugins/tpm/bin/update_plugins all
	end
# }}}
