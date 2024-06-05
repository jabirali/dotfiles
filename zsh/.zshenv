# ~/.zshenv: Zsh environment loaded by all shells.

# Command path
path=(
	# Personal
	~/.config/bin

    # Doom
    ~/.config/emacs/bin

	# Python environments
	/opt/intel/bin
	/opt/conda/bin

	# HomeBrew commands
	/opt/homebrew/bin

	# GNU environment
	/opt/homebrew/opt/coreutils/libexec/gnubin
	/opt/homebrew/opt/grep/libexec/gnubin

	# MacOS applications
	/Library/TeX/texbin
	/Applications/Matlab.app/bin

	# System defaults
	$path
)

# System settings
export COLORTERM="truecolor"
export EDITOR="emacs -nw"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

# Application settings
export BAT_STYLE="plain"
export BAT_THEME="ansi"
export DIRENV_LOG_FORMAT=
export MPLBACKEND="module://itermplot"
