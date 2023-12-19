# ~/.config/fish/config.fish: Config for the "Friendly Interactive Shell".
#
# See also:
# 	abbrfile        List of aliases that `fish-abbrfile` defines.
#	fish_plugins	List of packages to automatically install.

# Environment variables.
export EDITOR="subl -nw"
export DIRENV_LOG_FORMAT=

export BAT_STYLE="plain"
export BAT_THEME="ansi"

# Bootstrap the package manager.
if not functions -q fisher
	curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
	source ~/.config/fish/functions/fisher.fish
	fisher update
	clear
end

# Extra places to look for binaries.
fish_add_path ~/.config/bin
fish_add_path /usr/local/bin
fish_add_path /opt/homebrew/bin
fish_add_path /opt/homebrew/Caskroom/miniconda/base/bin/
fish_add_path /opt/conda/bin
fish_add_path /Library/TeX/texbin
fish_add_path /Applications/Matlab.app/bin

# Default command-line arguments.
alias exa 'exa --group-directories-first --time-style=long-iso'
alias ipython 'env TERM=linux ipython'  # ANSI colors
alias matlab 'matlab -nosplash -nodesktop'  # CLI mode
alias wget 'wget -e robots=off'  # Web scraping

# Direnv integration.
if type -q direnv
	eval (direnv hook fish)
	__direnv_export_eval
end

# Conda integration.
if type -q conda
	conda shell.fish hook | source
end

# iTerm2 integration.
if test -e ~/.iterm2_shell_integration.fish
	source ~/.iterm2_shell_integration.fish
end
