# ~/.config/fish/config.fish: Config for the "Friendly Interactive Shell".
#
# See also:
# 	abbrfile        List of aliases that `fish-abbrfile` defines.
#	fish_plugins	List of packages to automatically install.

# Environment variables.
set -x EDITOR subl

# Bootstrap the package manager.
if not functions -q fisher
	curl https://git.io/fisher --create-dirs -sLo ~/.config/fish/functions/fisher.fish
	source ~/.config/fish/functions/fisher.fish
	fisher update
	clear
end

# Extra places to look for binaries.
fish_add_path /usr/local/bin
fish_add_path /opt/homebrew/bin
fish_add_path /opt/homebrew/Caskroom/miniconda/base/bin/conda
fish_add_path /Library/TeX/texbin
fish_add_path /Applications/Matlab.app/bin
fish_add_path ~/.config/bin

# Direnv integration.
eval (direnv hook fish)
__direnv_export_eval

# Conda integration.
conda shell.fish hook | source

# Aliases for common actions.
alias bat    'bat -p --theme ansi'
alias exa    'exa --group-directories-first --time-style=long-iso'
alias matlab 'matlab -nosplash -nodesktop'
alias mkvenv 'echo layout_python >> .envrc; direnv allow'
alias entest 'fd \'.py$\' | entr pytest'
alias wget   'wget -e robots=off'
