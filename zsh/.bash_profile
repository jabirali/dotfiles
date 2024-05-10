# ~/.bash_profile: Minimal configuration for remote use.

# Default configuration.
if [ -f "$HOME/.bashrc" ]; then
	source ~/.bashrc
fi

# Change shell if possible.
if [ -x "$(command -v zsh)" ]; then
	export SHELL="$(which zsh)"
	exec zsh -l
fi
